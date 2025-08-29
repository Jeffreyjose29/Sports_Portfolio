# Football Match Prediction Model
# Advanced ML model using multiple features to predict match outcomes and scores
install.packages(c(
  "tidyverse", "randomForest", "xgboost", "caret", "corrplot", 
  "MASS", "nnet", "e1071", "glmnet", "pROC", "VIM"
))

library(tidyverse)      # Data manipulation
library(randomForest)   # Random Forest model
library(xgboost)       # XGBoost model
library(caret)         # ML framework
library(corrplot)      # Correlation plots
library(MASS)          # Negative binomial regression
library(nnet)          # Neural networks
library(e1071)         # SVM
library(glmnet)        # Elastic net
library(pROC)          # ROC curves
library(VIM)           # Missing value visualization

# Set seed for reproducibility
#set.seed(1111)

# Function to generate synthetic training data (replace with real data)
generate_training_data <- function(n = 5000) {
  data <- data.frame(
    # Team strength indicators
    home_elo = rnorm(n, 1500, 200),
    away_elo = rnorm(n, 1500, 200),
    home_squad_value = rlnorm(n, 18, 0.8),  # Squad value in millions
    away_squad_value = rlnorm(n, 18, 0.8),
    
    # Form (last 5 games points per game)
    home_form = runif(n, 0, 3),
    away_form = runif(n, 0, 3),
    
    # Head-to-head statistics
    h2h_home_wins = rpois(n, 2),
    h2h_away_wins = rpois(n, 2),
    h2h_draws = rpois(n, 1),
    h2h_home_goals_avg = runif(n, 0.5, 3),
    h2h_away_goals_avg = runif(n, 0.5, 3),
    
    # Recent performance metrics
    home_goals_scored_avg = runif(n, 0.8, 2.5),
    away_goals_scored_avg = runif(n, 0.8, 2.5),
    home_goals_conceded_avg = runif(n, 0.8, 2.5),
    away_goals_conceded_avg = runif(n, 0.8, 2.5),
    
    # Additional factors
    home_advantage = 1,  # Always 1 for home team
    days_since_last_match_home = rpois(n, 7),
    days_since_last_match_away = rpois(n, 7),
    competition_importance = sample(1:5, n, replace = TRUE),
    
    # Weather/external factors (normalized)
    weather_factor = runif(n, 0.8, 1.2),
    referee_strictness = runif(n, 0.5, 1.5)
  )
  
  # Calculate derived features
  data$elo_diff = data$home_elo - data$away_elo
  data$value_ratio = pmax(data$home_squad_value / data$away_squad_value, 0.1)  # Prevent division issues
  data$form_diff = data$home_form - data$away_form
  data$h2h_home_advantage = (data$h2h_home_wins - data$h2h_away_wins) / 
    (data$h2h_home_wins + data$h2h_away_wins + data$h2h_draws + 1)
  data$attack_strength_home = pmax(data$home_goals_scored_avg / data$away_goals_conceded_avg, 0.1)
  data$attack_strength_away = pmax(data$away_goals_scored_avg / data$home_goals_conceded_avg, 0.1)
  
  # Generate realistic outcomes based on features using improved probability calculation
  # Normalize strength indicators
  elo_diff_norm = pmax(pmin((data$elo_diff + 400) / 800, 1), 0)  # Normalize to 0-1
  value_ratio_norm = pmax(pmin(log(data$value_ratio + 1) / 4 + 0.5, 1), 0)  # Normalize to 0-1
  form_diff_norm = pmax(pmin((data$form_diff + 3) / 6, 1), 0)  # Normalize to 0-1
  h2h_norm = pmax(pmin(data$h2h_home_advantage + 0.5, 1), 0)  # Normalize to 0-1
  
  # Calculate base probabilities using logistic approach
  home_strength = 0.3 * elo_diff_norm + 0.25 * value_ratio_norm + 0.2 * form_diff_norm + 
    0.1 * h2h_norm + 0.15  # Home advantage bonus
  
  # Convert to probabilities using proper normalization
  # Base probabilities
  p_home_base = pmax(pmin(home_strength, 0.9), 0.1)  # Constrain between 0.1 and 0.9
  p_away_base = pmax(pmin(1 - home_strength, 0.9), 0.1)
  
  # Adjust for draws (football typically has ~25% draw rate)
  draw_factor = 0.25
  prob_home_win = p_home_base * (1 - draw_factor)
  prob_away_win = p_away_base * (1 - draw_factor)  
  prob_draw = rep(draw_factor, n)
  
  # Ensure probabilities sum to 1 and are all positive
  total_prob = prob_home_win + prob_away_win + prob_draw
  prob_home_win = prob_home_win / total_prob
  prob_away_win = prob_away_win / total_prob
  prob_draw = prob_draw / total_prob
  
  # Final safety check - ensure all probabilities are positive and sum to 1
  prob_home_win = pmax(prob_home_win, 0.05)
  prob_away_win = pmax(prob_away_win, 0.05) 
  prob_draw = pmax(prob_draw, 0.05)
  
  # Renormalize after safety check
  total_prob = prob_home_win + prob_away_win + prob_draw
  prob_home_win = prob_home_win / total_prob
  prob_away_win = prob_away_win / total_prob
  prob_draw = prob_draw / total_prob
  
  # Generate outcomes
  outcomes = sapply(1:n, function(i) {
    probs = c(prob_home_win[i], prob_draw[i], prob_away_win[i])
    # Additional safety check
    if(any(probs < 0) || sum(probs) == 0) {
      probs = c(0.45, 0.25, 0.3)  # Default probabilities
    }
    sample(c("H", "D", "A"), 1, prob = probs)
  })
  
  data$result = outcomes
  
  # Generate scores based on attack strength and result
  data$home_goals = sapply(1:n, function(i) {
    base_lambda = pmax(data$attack_strength_home[i], 0.3)
    if(data$result[i] == "H") {
      rpois(1, lambda = pmin(base_lambda * 1.8, 5))  # Cap at 5 for realism
    } else if(data$result[i] == "A") {
      rpois(1, lambda = pmin(base_lambda * 0.8, 3))
    } else {
      rpois(1, lambda = pmin(base_lambda * 1.2, 4))
    }
  })
  
  data$away_goals = sapply(1:n, function(i) {
    base_lambda = pmax(data$attack_strength_away[i], 0.3)
    if(data$result[i] == "A") {
      rpois(1, lambda = pmin(base_lambda * 1.8, 5))  # Cap at 5 for realism
    } else if(data$result[i] == "H") {
      rpois(1, lambda = pmin(base_lambda * 0.8, 3))
    } else {
      rpois(1, lambda = pmin(base_lambda * 1.2, 4))
    }
  })
  
  # Ensure draws have equal or close scores with better logic
  draw_indices = which(data$result == "D")
  for(i in draw_indices) {
    # For draws, make scores equal or within 1 goal
    avg_goals = round((data$home_goals[i] + data$away_goals[i]) / 2)
    if(runif(1) > 0.3) {  # 70% chance of equal score
      data$home_goals[i] = avg_goals
      data$away_goals[i] = avg_goals
    } else {  # 30% chance of 1-goal difference (still a draw in this context)
      if(runif(1) > 0.5) {
        data$home_goals[i] = avg_goals
        data$away_goals[i] = avg_goals
      } else {
        # Keep original scores if they're reasonable for a draw
        if(abs(data$home_goals[i] - data$away_goals[i]) > 1) {
          data$home_goals[i] = avg_goals
          data$away_goals[i] = avg_goals
        }
      }
    }
  }
  
  return(data)
}

# Function to preprocess features
preprocess_features <- function(data) {
  # Handle missing values
  data <- data %>%
    mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))
  
  # Calculate basic derived features first (if they don't exist)
  if(!"elo_diff" %in% names(data) && "home_elo" %in% names(data) && "away_elo" %in% names(data)) {
    data$elo_diff <- data$home_elo - data$away_elo
  }
  
  if(!"value_ratio" %in% names(data) && "home_squad_value" %in% names(data) && "away_squad_value" %in% names(data)) {
    data$value_ratio <- pmax(data$home_squad_value / data$away_squad_value, 0.1)
  }
  
  if(!"form_diff" %in% names(data) && "home_form" %in% names(data) && "away_form" %in% names(data)) {
    data$form_diff <- data$home_form - data$away_form
  }
  
  if(!"h2h_home_advantage" %in% names(data)) {
    data$h2h_home_advantage <- (data$h2h_home_wins - data$h2h_away_wins) / 
      (data$h2h_home_wins + data$h2h_away_wins + data$h2h_draws + 1)
  }
  
  if(!"attack_strength_home" %in% names(data)) {
    data$attack_strength_home <- pmax(data$home_goals_scored_avg / data$away_goals_conceded_avg, 0.1)
  }
  
  if(!"attack_strength_away" %in% names(data)) {
    data$attack_strength_away <- pmax(data$away_goals_scored_avg / data$home_goals_conceded_avg, 0.1)
  }
  
  # Create additional engineered features
  data <- data %>%
    mutate(
      total_h2h_games = h2h_home_wins + h2h_away_wins + h2h_draws,
      home_win_rate_h2h = ifelse(total_h2h_games > 0, h2h_home_wins / total_h2h_games, 0.33),
      goal_diff_tendency = h2h_home_goals_avg - h2h_away_goals_avg,
      rest_advantage = days_since_last_match_away - days_since_last_match_home,
      combined_elo = (home_elo + away_elo) / 2,
      combined_value = (home_squad_value + away_squad_value) / 2,
      elo_value_interaction = elo_diff * log(pmax(value_ratio, 0.1)),
      form_elo_interaction = form_diff * elo_diff / 100
    )
  
  return(data)
}

# Generate and preprocess training data
cat("Generating training data...\n")
train_data <- generate_training_data(8000)
train_data <- preprocess_features(train_data)

# Split into training and validation sets
train_idx <- createDataPartition(train_data$result, p = 0.8, list = FALSE)
train_set <- train_data[train_idx, ]
val_set <- train_data[-train_idx, ]

# Feature selection for models
feature_cols <- c("home_elo", "away_elo", "elo_diff", "home_squad_value", "away_squad_value", 
                  "value_ratio", "home_form", "away_form", "form_diff", "h2h_home_wins",
                  "h2h_away_wins", "h2h_draws", "home_win_rate_h2h", "h2h_home_goals_avg",
                  "h2h_away_goals_avg", "goal_diff_tendency", "home_goals_scored_avg",
                  "away_goals_scored_avg", "home_goals_conceded_avg", "away_goals_conceded_avg",
                  "attack_strength_home", "attack_strength_away", "home_advantage",
                  "rest_advantage", "competition_importance", "weather_factor",
                  "combined_elo", "elo_value_interaction", "form_elo_interaction")

# Model 1: XGBoost for match outcome prediction
cat("Training XGBoost model for match outcomes...\n")

# Prepare data for XGBoost
train_matrix <- xgb.DMatrix(
  data = as.matrix(train_set[, feature_cols]),
  label = as.numeric(as.factor(train_set$result)) - 1  # 0=A, 1=D, 2=H
)

val_matrix <- xgb.DMatrix(
  data = as.matrix(val_set[, feature_cols]),
  label = as.numeric(as.factor(val_set$result)) - 1
)

# XGBoost parameters (well-tuned)
xgb_params <- list(
  objective = "multi:softprob",
  num_class = 3,
  eval_metric = "mlogloss",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 3,
  subsample = 0.8,
  colsample_bytree = 0.8,
  lambda = 1,
  alpha = 0,
  seed = 42
)

# Train XGBoost model
xgb_model <- xgb.train(
  params = xgb_params,
  data = train_matrix,
  nrounds = 200,
  watchlist = list(train = train_matrix, val = val_matrix),
  early_stopping_rounds = 20,
  verbose = 1
)

# Model 2: Random Forest for additional ensemble
cat("Training Random Forest model...\n")
rf_model <- randomForest(
  x = train_set[, feature_cols],
  y = as.factor(train_set$result),
  ntree = 500,
  mtry = sqrt(length(feature_cols)),
  importance = TRUE,
  nodesize = 5
)

# Model 3: Poisson models for score prediction
cat("Training Poisson models for score prediction...\n")

# Prepare features for score prediction
score_features <- c(feature_cols, "result")  # Include result as feature for score prediction

# Home goals model
home_goals_model <- glm(
  home_goals ~ .,
  data = train_set[, c("home_goals", score_features)],
  family = poisson()
)

# Away goals model  
away_goals_model <- glm(
  away_goals ~ .,
  data = train_set[, c("away_goals", score_features)],
  family = poisson()
)

# Model 4: Neural Network for complex interactions
cat("Training Neural Network model...\n")
nn_model <- nnet(
  as.factor(result) ~ .,
  data = train_set[, c("result", feature_cols)],
  size = 10,
  decay = 0.01,
  MaxNWts = 2000,
  trace = FALSE
)

# Function to make predictions
predict_match <- function(home_team_data, away_team_data, models) {
  # Combine team data into match features
  match_data <- data.frame(
    home_elo = home_team_data$elo,
    away_elo = away_team_data$elo,
    home_squad_value = home_team_data$squad_value,
    away_squad_value = away_team_data$squad_value,
    home_form = home_team_data$form,
    away_form = away_team_data$form,
    h2h_home_wins = home_team_data$h2h_wins,
    h2h_away_wins = away_team_data$h2h_wins,
    h2h_draws = home_team_data$h2h_draws,
    h2h_home_goals_avg = home_team_data$h2h_goals_for,
    h2h_away_goals_avg = away_team_data$h2h_goals_for,
    home_goals_scored_avg = home_team_data$goals_scored_avg,
    away_goals_scored_avg = away_team_data$goals_scored_avg,
    home_goals_conceded_avg = home_team_data$goals_conceded_avg,
    away_goals_conceded_avg = away_team_data$goals_conceded_avg,
    home_advantage = 1,
    days_since_last_match_home = home_team_data$days_since_last_match,
    days_since_last_match_away = away_team_data$days_since_last_match,
    competition_importance = home_team_data$competition_importance,
    weather_factor = 1.0,
    referee_strictness = 1.0
  )
  
  # Preprocess match data (this will calculate all derived features)
  match_data <- preprocess_features(match_data)
  
  # Ensure all required features exist with default values if missing
  required_features <- c("home_elo", "away_elo", "elo_diff", "home_squad_value", "away_squad_value", 
                         "value_ratio", "home_form", "away_form", "form_diff", "h2h_home_wins",
                         "h2h_away_wins", "h2h_draws", "home_win_rate_h2h", "h2h_home_goals_avg",
                         "h2h_away_goals_avg", "goal_diff_tendency", "home_goals_scored_avg",
                         "away_goals_scored_avg", "home_goals_conceded_avg", "away_goals_conceded_avg",
                         "attack_strength_home", "attack_strength_away", "home_advantage",
                         "rest_advantage", "competition_importance", "weather_factor",
                         "combined_elo", "elo_value_interaction", "form_elo_interaction")
  
  # Add any missing features with default values
  for(feature in required_features) {
    if(!feature %in% names(match_data)) {
      match_data[[feature]] <- 0  # Default to 0 for missing features
    }
  }
  
  # Get predictions from all models
  # XGBoost prediction
  xgb_pred <- predict(models$xgb, as.matrix(match_data[, feature_cols]))
  xgb_probs <- matrix(xgb_pred, nrow = 1, byrow = TRUE)
  
  # Random Forest prediction
  rf_pred <- predict(models$rf, match_data[, feature_cols], type = "prob")
  
  # Neural Network prediction
  nn_pred <- predict(models$nn, match_data[, feature_cols], type = "raw")
  
  # Ensure all predictions have the same format (3 probabilities)
  if(length(xgb_probs) != 3) xgb_probs <- c(0.3, 0.25, 0.45)  # Default probabilities
  if(length(rf_pred) != 3) rf_pred <- c(0.3, 0.25, 0.45)
  if(length(nn_pred) != 3) nn_pred <- c(0.3, 0.25, 0.45)
  
  # Ensure probabilities are properly formatted as vectors
  xgb_probs <- as.numeric(xgb_probs)
  rf_pred <- as.numeric(rf_pred)
  nn_pred <- as.numeric(nn_pred)
  
  # Ensemble prediction (weighted average)
  ensemble_probs <- 0.5 * xgb_probs + 0.3 * rf_pred + 0.2 * nn_pred
  
  # Normalize probabilities to sum to 1
  ensemble_probs <- ensemble_probs / sum(ensemble_probs)
  names(ensemble_probs) <- c("Away Win", "Draw", "Home Win")
  
  # Predict most likely outcome
  predicted_outcome <- names(ensemble_probs)[which.max(ensemble_probs)]
  
  # Predict scores
  # First predict the most likely result for score models
  likely_result <- c("A", "D", "H")[which.max(ensemble_probs)]
  match_data$result <- likely_result
  
  # Predict goals
  home_goals_pred <- predict(models$home_goals, match_data, type = "response")
  away_goals_pred <- predict(models$away_goals, match_data, type = "response")
  
  # Round to nearest integer and ensure realistic bounds
  predicted_home_goals <- max(0, min(6, round(home_goals_pred)))
  predicted_away_goals <- max(0, min(6, round(away_goals_pred)))
  
  # Return comprehensive prediction
  return(list(
    outcome_probabilities = ensemble_probs,
    predicted_outcome = predicted_outcome,
    predicted_score = paste(predicted_home_goals, "-", predicted_away_goals),
    home_goals = predicted_home_goals,
    away_goals = predicted_away_goals,
    confidence = max(ensemble_probs)
  ))
}

# Evaluate models
cat("Evaluating models...\n")

# XGBoost evaluation
xgb_val_pred <- predict(xgb_model, val_matrix)
xgb_val_pred_class <- max.col(matrix(xgb_val_pred, ncol = 3, byrow = TRUE)) - 1
xgb_accuracy <- mean(xgb_val_pred_class == (as.numeric(as.factor(val_set$result)) - 1))

# Random Forest evaluation
rf_val_pred <- predict(rf_model, val_set[, feature_cols])
rf_accuracy <- mean(rf_val_pred == val_set$result)

cat(sprintf("XGBoost Accuracy: %.3f\n", xgb_accuracy))
cat(sprintf("Random Forest Accuracy: %.3f\n", rf_accuracy))

# Feature importance
cat("Top 10 Most Important Features (XGBoost):\n")
importance_matrix <- xgb.importance(feature_names = feature_cols, model = xgb_model)
print(importance_matrix[1:10])

# Store models
models <- list(
  xgb = xgb_model,
  rf = rf_model,
  home_goals = home_goals_model,
  away_goals = away_goals_model,
  nn = nn_model
)

# Example usage function
example_prediction <- function() {
  # Example team data
  home_team <- list(
    elo = 2003,
    squad_value = 1050,  # millions
    form = 3.0,  # points per game last 5
    h2h_wins = 2, #Last 5- 10 meetings (max 3-4 years)
    h2h_draws = 2, #Last 5- 10 meetings (max 3-4 years)
    h2h_goals_for = 9, #Last 5- 10 meetings (max 3-4 years)
    goals_scored_avg = 2.3, #Lookback: Last 10-15 matches (2-3 months of games)
    goals_conceded_avg = 1.8, #Lookback: Last 10-15 matches (2-3 months of games)
    days_since_last_match = 6,
    competition_importance = 3
    
    #1: Friendly/meaningless game
    #2: Regular league match (safe position)
    #3: Important league match
    #4: Cup match/European competition ✅ (Your match here)
    #5: Cup final/decisive match/relegation battle
  )
  
  away_team <- list(
    elo = 2002,
    squad_value = 1370,
    form = 3.0,
    h2h_wins = 1,
    h2h_draws = 2,
    h2h_goals_for = 8,
    goals_scored_avg = 2.0,
    goals_conceded_avg = 0.9,
    days_since_last_match = 8,
    competition_importance = 3
  )
  
  prediction <- predict_match(home_team, away_team, models)
  
  cat("\n=== MATCH PREDICTION ===\n")
  cat("Outcome Probabilities:\n")
  for(i in 1:length(prediction$outcome_probabilities)) {
    cat(sprintf("%s: %.1f%%\n", names(prediction$outcome_probabilities)[i], 
                prediction$outcome_probabilities[i] * 100))
  }
  cat(sprintf("Predicted Outcome: %s\n", prediction$predicted_outcome))
  cat(sprintf("Predicted Score: %s\n", prediction$predicted_score))
  cat(sprintf("Confidence: %.1f%%\n", prediction$confidence * 100))
  
  return(prediction)
}

# Run example
cat("\nRunning example prediction...\n")
example_result <- example_prediction()

# Additional utility functions
save_model <- function(models, filename) {
  saveRDS(models, file = filename)
  cat(sprintf("Models saved to %s\n", filename))
}

load_model <- function(filename) {
  models <- readRDS(filename)
  cat(sprintf("Models loaded from %s\n", filename))
  return(models)
}

# Save the trained models
# save_model(models, "football_prediction_models.rds")

cat("\n=== MODEL TRAINING COMPLETE ===\n")
cat("Use predict_match(home_team_data, away_team_data, models) to make predictions\n")
cat("Models include: XGBoost, Random Forest, Poisson regression, and Neural Network\n")
