## Package names
packages <- c("dplyr", "readxl", "purrr", "sjmisc", "worldfootballR", "plyr", "magick", "httr", "jsonlite", "kableExtra", "webshot", "ggplot2",
              "stringr", "StatsBombR", "ggsoccer")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))


## 1. Get match request API 
match_id <- "4506301"
request_id <- GET(paste0("https://www.fotmob.com/api/matchDetails?matchId=", match_id))

content <- fromJSON(rawToChar(request_id$content))


# 2. Format the file

# 2.2 Variable Names

league_name <- paste(content[["content"]][["matchFacts"]][["infoBox"]][["Tournament"]][["leagueName"]], content[["content"]][["matchFacts"]][["infoBox"]][["Tournament"]][["selectedSeason"]])
league_round_name <- content[["content"]][["matchFacts"]][["infoBox"]][["Tournament"]][["roundName"]]

home_team <- content[["general"]][["homeTeam"]][["name"]]
away_team <- content[["general"]][["awayTeam"]][["name"]]

home_team_id <- content[["general"]][["homeTeam"]][["id"]]
away_team_id <- content[["general"]][["awayTeam"]][["id"]]

home_team_colours <- content[["general"]][["teamColors"]][["darkMode"]][["home"]]
away_team_colours <- content[["general"]][["teamColors"]][["darkMode"]][["away"]]

home_team_logo <- content[["header"]][["teams"]][["imageUrl"]][1]
away_team_logo <- content[["header"]][["teams"]][["imageUrl"]][2]

home_team_score <- content[["header"]][["teams"]][["score"]][1]
away_team_score <- content[["header"]][["teams"]][["score"]][2]

match_time_utc <- content[["general"]][["matchTimeUTC"]]

final_score <- content[["header"]][["status"]][["scoreStr"]]

reason_long <- content[["header"]][["status"]][["reason"]][["long"]]

redCardHome <- as.data.frame(content[["header"]][["status"]][["homeRedCards"]])
redCardAway <- as.data.frame(content[["header"]][["status"]][["awayRedCards"]])


# Red Card Dataset
if(nrow(redCardHome) > 0 && nrow(redCardAway) == 0){
  redCardHistory <- redCardHome %>%
    mutate(home_away = "home")
} else if(nrow(redCardAway) > 0 && nrow(redCardHome) == 0){
  redCardHistory <- redCardAway %>%
    mutate(home_away = "away")
} else if(nrow(redCardAway) > 0 && nrow(redCardHome) > 0){
  redCardHistory <- rbind(
    as.data.frame(content[["header"]][["status"]][["homeRedCards"]]) %>%
      mutate(home_away = "home"),
    as.data.frame(content[["header"]][["status"]][["awayRedCards"]]) %>%
      mutate(home_away = "away")
  )
}else{redCardHistory <- data.frame()}



# Goals 
home_team_goals <- map(content[["header"]][["events"]]$homeTeamGoals, function(xx) xx %>% select(!any_of(c("timeStr", "overloadTimeStr"))))
home_team_goals <- bind_rows(home_team_goals) %>% 
  mutate(home_away = "home")
away_team_goals <- map(content[["header"]][["events"]]$awayTeamGoals, function(xx) xx %>% select(!any_of(c("timeStr", "overloadTimeStr"))))
away_team_goals <- bind_rows(away_team_goals) %>% 
  mutate(home_away = "away")


# Match Facts
stadium <- as.data.frame(content[["content"]][["matchFacts"]][["infoBox"]][["Stadium"]])
referee <- as.data.frame(content[["content"]][["matchFacts"]][["infoBox"]][["Referee"]])
match_attendance <- content[["content"]][["matchFacts"]][["infoBox"]][["Attendance"]]

## Form
home_team_form <- as.data.frame(content[["content"]][["matchFacts"]][["teamForm"]][1]) %>%
  mutate(home_away = "home")
away_team_form <- as.data.frame(content[["content"]][["matchFacts"]][["teamForm"]][2]) %>%
  mutate(home_away = "away")


## Top Players
home_team_top_players <- as.data.frame(content[["content"]][["matchFacts"]][["topPlayers"]][["homeTopPlayers"]])
away_team_top_players <- as.data.frame(content[["content"]][["matchFacts"]][["topPlayers"]][["awayTopPlayers"]])


# Top Scorers 
top_scorers <- unique(as.data.frame(content[["content"]][["matchFacts"]][["topScorers"]]))


# Momentum (- Is for Away Team)
if(length(content[["content"]][["matchFacts"]][["momentum"]][["main"]]) != 0){
  momentum <- as.data.frame(content[["content"]][["matchFacts"]][["momentum"]][["main"]]) %>%
    mutate(home_away = if_else(data.value < 0, "away", "home"),
           home_away_1 = if_else(data.value < 0, away_team_colours, home_team_colours))
}else{print("no")}


# Match Stats - TBD
all_stats <- as.data.frame(content[["content"]][["stats"]][["Periods"]][["All"]][["stats"]])
first_half_stats <- as.data.frame(content[["content"]][["stats"]][["Periods"]][["FirstHalf"]][["stats"]])
second_half_stats <- as.data.frame(content[["content"]][["stats"]][["Periods"]][["SecondHalf"]][["stats"]])



# Shot Map
all_shots <- as.data.frame(content[["content"]][["shotmap"]][["shots"]])
first_half_shots <- as.data.frame(content[["content"]][["shotmap"]][["Periods"]][["FirstHalf"]])
second_half_shots <- as.data.frame(content[["content"]][["shotmap"]][["Periods"]][["SecondHalf"]])


## Line Up

## Formation

home_team_formation <- content[["content"]][["lineup"]]$homeTeam$formation
away_team_formation <- content[["content"]][["lineup"]]$awayTeam$formation

home_team_rating <- content[["content"]][["lineup"]]$homeTeam$rating
away_team_rating <- content[["content"]][["lineup"]]$awayTeam$rating


home_team_starters <- content[["content"]][["lineup"]]$homeTeam$starters

home_player_events <- data.frame()
for(i in 1:nrow(home_team_starters)){
  
  if(!is.null(content[["content"]][["lineup"]][["homeTeam"]][["starters"]][["performance"]][["events"]][[i]]) &&
     nrow(content[["content"]][["lineup"]][["homeTeam"]][["starters"]][["performance"]][["events"]][[i]]) > 0){
    player_id <- home_team_starters[i, 1]
    player_name <- home_team_starters[i, 3]
    
    events_tmp <- as.data.frame(content[["content"]][["lineup"]][["homeTeam"]][["starters"]][["performance"]][["events"]][[i]])
    
    events_tmp$player_id <- player_id
    events_tmp$player_name <- player_name
    
    home_player_events <- rbind(home_player_events, events_tmp)
  }
}

away_team_starters <- content[["content"]][["lineup"]]$awayTeam$starters

away_player_events <- data.frame()
for(i in 1:nrow(away_team_starters)){
  
  if(!is.null(content[["content"]][["lineup"]][["awayTeam"]][["starters"]][["performance"]][["events"]][[i]]) &&
     nrow(content[["content"]][["lineup"]][["awayTeam"]][["starters"]][["performance"]][["events"]][[i]]) > 0){
    player_id <- away_team_starters[i, 1]
    player_name <- away_team_starters[i, 3]
    
    events_tmp <- as.data.frame(content[["content"]][["lineup"]][["awayTeam"]][["starters"]][["performance"]][["events"]][[i]])
    
    events_tmp$player_id <- player_id
    events_tmp$player_name <- player_name
    
    away_player_events <- rbind(away_player_events, events_tmp)
  }
}


home_team_subs <- content[["content"]][["lineup"]]$homeTeam$subs
away_team_subs <- content[["content"]][["lineup"]]$awayTeam$subs


## Match 2 Lineup
home_team_average_age <- content[["content"]][["match2Lineup"]][["homeTeam"]][["averageStarterAge"]]
away_team_average_age <- content[["content"]][["match2Lineup"]][["awayTeam"]][["averageStarterAge"]]


home_team_starter_info <- as.data.frame(content[["content"]][["match2Lineup"]][["homeTeam"]][["starters"]])
away_team_starter_info <- as.data.frame(content[["content"]][["match2Lineup"]][["awayTeam"]][["starters"]])


home_team_sub_info <- as.data.frame(content[["content"]][["match2Lineup"]][["homeTeam"]][["subs"]])
away_team_sub_info <- as.data.frame(content[["content"]][["match2Lineup"]][["awayTeam"]][["subs"]])


home_team_coach_info <- as.data.frame(
  list(
    Id = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$id %||% NA,
    Age = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$age %||% NA,
    Name = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$name %||% NA,
    CountryName = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$countryName %||% NA,
    CountryCode = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$countryCode %||% NA,
    FirstName = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$firstName %||% NA,
    LastName = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$lastName %||% NA,
    PrimaryTeamID = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$primaryTeamId %||% NA,
    PrimaryTeamName = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$primaryTeamName %||% NA,
    UsualPlayingPositionID = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$usualPlayingPositionId %||% NA,
    IsCoach = content[["content"]][["match2Lineup"]][["homeTeam"]][["coach"]]$isCoach %||% NA
  )
)

away_team_coach_info <- as.data.frame(
  list(
    Id = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$id %||% NA,
    Age = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$age %||% NA,
    Name = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$name %||% NA,
    CountryName = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$countryName %||% NA,
    CountryCode = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$countryCode %||% NA,
    FirstName = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$firstName %||% NA,
    LastName = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$lastName %||% NA,
    PrimaryTeamID = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$primaryTeamId %||% NA,
    PrimaryTeamName = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$primaryTeamName %||% NA,
    UsualPlayingPositionID = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$usualPlayingPositionId %||% NA,
    IsCoach = content[["content"]][["match2Lineup"]][["awayTeam"]][["coach"]]$isCoach %||% NA
  )
)


# Define `%||%` to provide a default value
`%||%` <- function(x, y) ifelse(is.null(x), y, x)

coach_info <- rbind(home_team_coach_info, away_team_coach_info)

## Head To Head
#head_2_head <- as.data.frame(content[["content"]][["h2h"]][["matches"]])

### Visualization

canvas_colour <- "#282828"
main_text_colour <- "#FFFFFF"
tertiary_colour <- "#C6C6C3"

soccer_ball_logo <- image_read("https://static.vecteezy.com/system/resources/previews/015/276/951/original/soccer-ball-illustration-icon-sport-element-free-png.png")
soccer_ball_logo <- image_scale(soccer_ball_logo, "40x40")

red_card_logo <- image_read("https://i.ibb.co/J2XzYyc/imageedit-1-8329203682.png")
red_card_logo <- image_scale(red_card_logo, "45x60")

yellow_card_logo <- image_read("https://i.ibb.co/S3x8X7L/imageedit-2-4574879790.png")
yellow_card_logo <- image_scale(yellow_card_logo, "45x60")


# Goals scorers

if(nrow(home_team_goals) > 0){
  home_team_goals_temp <- home_team_goals %>%
    select(fullName, time) %>%
    arrange(time) %>%
    mutate(scorer = paste(fullName, "     ", time, "'")) %>%
    select(scorer)
  
  home_team_goals_temp <- kable(home_team_goals_temp, escape = FALSE, format = "html", align = 'r', col.names = NULL, booktabs = T) %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), position = "right", full_width = F) %>%
    # Row Specific
    row_spec(1:nrow(home_team_goals_temp), background = canvas_colour, color = tertiary_colour, align = "right",
             extra_css = "border: none;") %>%
    kable_styling()
  
  # Save the table as an HTML file
  home_goals_html_file <- "home_goals.html"
  save_kable(home_team_goals_temp, home_goals_html_file)
  
  # Convert the HTML file to an image
  home_goals_img_file <- "home_goals.png"
  webshot(home_goals_html_file, file = home_goals_img_file, selector = "table", zoom = 1.4)
  
  # Read the image with magick
  home_goals_img <- image_read(home_goals_img_file)
}


if(nrow(away_team_goals) > 0){
  away_team_goals_temp <- away_team_goals %>%
    select(fullName, time) %>%
    arrange(time) %>%
    mutate(scorer = paste(fullName, "     ", time, "'")) %>%
    select(scorer)
  
  away_team_goals_temp <- kable(away_team_goals_temp, escape = FALSE, format = "html", align = 'l', col.names = NULL, booktabs = T) %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), position = "left", full_width = F) %>%
    # Row Specific
    row_spec(1:nrow(away_team_goals_temp), background = canvas_colour, color = tertiary_colour, align = "left",
             extra_css = "border: none;") 
  
  # Save the table as an HTML file
  away_goals_html_file <- "away_goals.html"
  save_kable(away_team_goals_temp, away_goals_html_file)
  
  # Convert the HTML file to an image
  away_goals_img_file <- "away_goals.png"
  webshot(away_goals_html_file, file = away_goals_img_file, selector = "table", zoom = 1.4)
  
  # Read the image with magick
  away_goals_img <- image_read(away_goals_img_file)
}


## Momentum Chart
if(length(content[["content"]][["matchFacts"]][["momentum"]][["main"]]) != 0){
  momentum_chart <- momentum %>%
    ggplot(aes(x = data.minute, y = data.value, fill = home_away_1)) + geom_col() +
    scale_fill_identity() +
    theme_minimal() +
    theme(panel.background = element_rect(fill = canvas_colour, color = NA),
          plot.background = element_rect(fill = canvas_colour, color = NA),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(colour = main_text_colour, size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") 
  
  gg_grob <- ggplotGrob(momentum_chart)
  gg_image <- image_graph(width = 2000, height = 300, res = 98)
  grid::grid.draw(gg_grob)
  dev.off()
}


## Red Cards
if(nrow(redCardHistory) > 0){
  red_card_event_id <- unique(redCardHistory$eventId)
  
  if(nrow(redCardHome) > 0){
    home_red_cards <- redCardHistory %>%
      filter(home_away == "home") %>%
      select(time, nameStr) %>%
      arrange(time) %>%
      mutate(redCarder = paste(nameStr, "     ", time, "'")) %>%
      select(redCarder)
  }else{home_red_cards <- data.frame()}
  
  if(nrow(redCardAway) > 0){
    away_red_cards <- redCardHistory %>%
      filter(home_away == "away") %>%
      select(time, nameStr) %>%
      arrange(time) %>%
      mutate(redCarder = paste(nameStr, "     ", time, "'")) %>%
      select(redCarder)
  }else{away_red_cards <- data.frame()}
  
  if(nrow(home_red_cards) > 0){
    home_team_red_card_temp <- kable(home_red_cards, escape = FALSE, format = "html", align = 'l', col.names = NULL, booktabs = T) %>%
      kable_styling(bootstrap_options = c("striped", "condensed"), position = "left", full_width = F) %>%
      # Row Specific
      row_spec(1:nrow(home_red_cards), background = canvas_colour, color = tertiary_colour, align = "left",
               extra_css = "border: none;") %>%
      kable_styling()
    
    # Save the table as an HTML file
    home_red_card_html_file <- "home_red_card.html"
    save_kable(home_team_red_card_temp, home_red_card_html_file)
    
    # Convert the HTML file to an image
    home_red_card_img_file <- "home_red_card.png"
    webshot(home_red_card_html_file, file = home_red_card_img_file, selector = "table", zoom = 1.4)
    
    # Read the image with magick
    home_red_card_img <- image_read(home_red_card_img_file)
  }else{}
  
  
  if(nrow(away_red_cards) > 0){
    away_team_red_card_temp <- kable(away_red_cards, escape = FALSE, format = "html", align = 'r', col.names = NULL, booktabs = T) %>%
      kable_styling(bootstrap_options = c("striped", "condensed"), position = "right", full_width = F) %>%
      # Row Specific
      row_spec(1:nrow(away_red_cards), background = canvas_colour, color = tertiary_colour, align = "right",
               extra_css = "border: none;") %>%
      kable_styling()
    
    # Save the table as an HTML file
    away_red_card_html_file <- "away_red_card.html"
    save_kable(away_team_red_card_temp, away_red_card_html_file)
    
    # Convert the HTML file to an image
    away_red_card_img_file <- "away_red_card.png"
    webshot(away_red_card_html_file, file = away_red_card_img_file, selector = "table", zoom = 1.4)
    
    # Read the image with magick
    away_red_card_img <- image_read(away_red_card_img_file)
  }else{}
}


## Match Events
match_events <- data.frame(content[["content"]][["matchFacts"]]$events$events)
if(nrow(redCardHistory) > 0){
  match_events <- match_events %>%
    filter(!eventId %in% red_card_event_id)
}

match_events_cards <- match_events %>%
  filter(type == "Card")

home_yellow_card <- match_events_cards %>%
  filter(isHome == TRUE)

away_yellow_card <- match_events_cards %>%
  filter(isHome == FALSE)

if(nrow(match_events_cards) > 0){
  
  if(nrow(home_yellow_card) > 0){
    home_yellow_cards <- match_events_cards %>%
      filter(isHome == TRUE) %>%
      select(time, nameStr) %>%
      arrange(time) %>% 
      mutate(yellowCarder = paste(nameStr, "     ", time, "'")) %>%
      select(yellowCarder)
  }
  
  if(nrow(away_yellow_card) > 0){
    away_yellow_cards <- match_events_cards %>%
      filter(isHome == FALSE) %>%
      select(time, nameStr) %>%
      arrange(time) %>% 
      mutate(yellowCarder = paste(nameStr, "     ", time, "'")) %>%
      select(yellowCarder)
  }
  
  if(nrow(home_yellow_card) > 0){
    home_team_yellow_card_temp <- kable(home_yellow_cards, escape = FALSE, format = "html", align = 'l', col.names = NULL, booktabs = T) %>%
      kable_styling(bootstrap_options = c("striped", "condensed"), position = "left", full_width = F) %>%
      # Row Specific
      row_spec(1:nrow(home_yellow_cards), background = canvas_colour, color = tertiary_colour, align = "left",
               extra_css = "border: none;") %>%
      kable_styling()
    
    # Save the table as an HTML file
    home_yellow_card_html_file <- "home_yellow_card.html"
    save_kable(home_team_yellow_card_temp, home_yellow_card_html_file)
    
    # Convert the HTML file to an image
    home_yellow_card_img_file <- "home_yellow_card.png"
    webshot(home_yellow_card_html_file, file = home_yellow_card_img_file, selector = "table", zoom = 1.4)
    
    # Read the image with magick
    home_yellow_card_img <- image_read(home_yellow_card_img_file)
  }else{}
  
  if(nrow(away_yellow_card) > 0){
    away_team_yellow_card_temp <- kable(away_yellow_cards, escape = FALSE, format = "html", align = 'r', col.names = NULL, booktabs = T) %>%
      kable_styling(bootstrap_options = c("striped", "condensed"), position = "right", full_width = F) %>%
      # Row Specific
      row_spec(1:nrow(away_yellow_cards), background = canvas_colour, color = tertiary_colour, align = "right",
               extra_css = "border: none;") %>%
      kable_styling()
    
    # Save the table as an HTML file
    away_yellow_card_html_file <- "away_yellow_card.html"
    save_kable(away_team_yellow_card_temp, away_yellow_card_html_file)
    
    # Convert the HTML file to an image
    away_yellow_card_img_file <- "away_yellow_card.png"
    webshot(away_yellow_card_html_file, file = away_yellow_card_img_file, selector = "table", zoom = 1.4)
    
    # Read the image with magick
    away_yellow_card_img <- image_read(away_yellow_card_img_file)
  }else{}
}


## Stats
i_value <- length(content[["content"]][["stats"]][["Periods"]][["All"]][["stats"]][["stats"]])
match_stats <- data.frame()
for(i in 1:i_value){
  match_stats_tmp <- content[["content"]][["stats"]][["Periods"]][["All"]][["stats"]][["stats"]][[i]]
  
  match_stats <- rbind(match_stats, match_stats_tmp)
}

match_stats$IS_PERCENTAGE <- grepl("%", match_stats$stats, fixed = TRUE)

match_stats[c('Home Stat', 'Away Stat')] <- str_split_fixed(match_stats$stats, "[,:]", 2)
match_stats$`Home Stat` <- gsub("[^[:digit:]., ]", "", match_stats$`Home Stat`)
match_stats$`Away Stat` <- gsub("[^[:digit:]., ]", "", match_stats$`Away Stat`)
match_stats$`Home Stat` <- trimws(match_stats$`Home Stat`, which = "left", whitespace = "[ \t\r\n]" )
match_stats$`Away Stat` <- trimws(match_stats$`Away Stat`, which = "both", whitespace = "[ \t\r\n]" )


# Extract values after spaces, return original if no space
match_stats$Home_Percentage <- ifelse(grepl(" ", match_stats$`Home Stat`), sub(".* ", "", match_stats$`Home Stat`), "")
match_stats$Away_Percentage <- ifelse(grepl(" ", match_stats$`Away Stat`), sub(".* ", "", match_stats$`Away Stat`), "")


# Remove all values after the first space
match_stats$`Home Stat` <- sub(" .*", "", match_stats$`Home Stat`)
match_stats$`Away Stat` <- sub(" .*", "", match_stats$`Away Stat`)

## Formatting the showcase columns
match_stats$Home_Stat_Formatted <- ifelse(is.na(match_stats$Home_Percentage) | match_stats$Home_Percentage == "", match_stats$`Home Stat`, 
                                          paste0(match_stats$`Home Stat`, " (", match_stats$Home_Percentage, "%)"))

match_stats$Away_Stat_Formatted <- ifelse(is.na(match_stats$Away_Percentage) | match_stats$Away_Percentage == "", match_stats$`Away Stat`, 
                                          paste0(match_stats$`Away Stat`, " (", match_stats$Away_Percentage, "%)"))

match_stats$Home_Stat_Formatted <- trimws(match_stats$Home_Stat_Formatted, which = "both", whitespace = "[ \t\r\n]" )
match_stats$Away_Stat_Formatted <- trimws(match_stats$Away_Stat_Formatted, which = "both", whitespace = "[ \t\r\n]" )

match_stats_showcase <- match_stats %>%
  select(Home_Stat_Formatted, title, Away_Stat_Formatted) %>%
  filter(title %in% c("Ball possession", "Expected goals (xG)", "Total shots", "Shots on target", "Big chances", "Big chances missed",
                      "Accurate passes", "Corners", "xG open play", "xG set play", "xG on target (xGOT)", "Goals Prevented"))

match_stats_showcase <- unique(match_stats_showcase)

match_stats_showcase <- match_stats_showcase %>%
  distinct(title, .keep_all = TRUE)

colnames(match_stats_showcase)[1] <- home_team
colnames(match_stats_showcase)[3] <- away_team
colnames(match_stats_showcase)[2] <- ""

match_stats_card <- kable(match_stats_showcase, escape = FALSE, format = "html", align = 'c', booktabs = TRUE) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    position = "center",
    full_width = FALSE,
    html_font = "Arial"
  ) %>%
  row_spec(0, background = canvas_colour, extra_css = "border: none;") %>%
  row_spec(1:nrow(match_stats_showcase), align = "center", background = canvas_colour, extra_css = "border: none;") %>%
  column_spec(1, background = canvas_colour, color = home_team_colours, width = "9em", include_thead = TRUE, bold = TRUE, extra_css = "border: none;") %>%
  column_spec(2, background = canvas_colour, color = tertiary_colour, width = "9em", include_thead = TRUE, bold = TRUE, extra_css = "border: none;") %>%
  column_spec(3, background = canvas_colour, color = away_team_colours, width = "9em", include_thead = TRUE, bold = TRUE, extra_css = "border: none;")


# Save the table as an HTML file
match_stats_card_html <- "match_stats_card_html.html"
save_kable(match_stats_card, match_stats_card_html)

# Convert the HTML file to an image
match_stats_card_img_file <- "match_stats_card.png"
webshot(match_stats_card_html, file = match_stats_card_img_file, selector = "table", zoom = 1.5)

# Read the image with magick
match_stats_card_img <- image_read(match_stats_card_img_file)


## Form Charts
if(nrow(home_team_form) > 0){
  home_team_form$fill_colour <- ifelse(home_team_form$resultString == "W", "#00985D",
                                       ifelse(home_team_form$resultString == "L", "#DC3638", "#8A9597"))
  home_team_form$max_value <- 1
  home_team_form$sequence_order <- as.factor(seq(1:5))
  
  home_form <- home_team_form %>%
    ggplot(aes(x = max_value, y = sequence_order, fill = fill_colour, group = fill_colour)) + geom_bar(stat = "identity", position = "dodge") +
    theme_void() + scale_fill_identity() +
    theme(panel.background = element_rect(fill = canvas_colour, color = NA),
          plot.background = element_rect(fill = canvas_colour, color = NA),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  gg_grob_home_form <- ggplotGrob(home_form)
  gg_image_home_form <- image_graph(width = 50, height = 250, res = 98)
  grid::grid.draw(gg_grob_home_form)
  dev.off()
}
if(nrow(away_team_form) > 0){
  away_team_form$fill_colour <- ifelse(away_team_form$resultString == "W", "#00985D",
                                       ifelse(away_team_form$resultString == "L", "#DC3638", "#8A9597"))
  away_team_form$max_value <- 1
  away_team_form$sequence_order <- as.factor(seq(1:5))
  
  away_form <- away_team_form %>%
    ggplot(aes(x = max_value, y = sequence_order, fill = fill_colour, group = fill_colour)) + geom_bar(stat = "identity", position = "dodge") +
    theme_void() + scale_fill_identity() +
    theme(panel.background = element_rect(fill = canvas_colour, color = NA),
          plot.background = element_rect(fill = canvas_colour, color = NA),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  gg_grob_away_form <- ggplotGrob(away_form)
  gg_image_away_form <- image_graph(width = 50, height = 250, res = 98)
  grid::grid.draw(gg_grob_away_form)
  dev.off()
}


### Shot Map
if(nrow(all_shots) > 0){
  home_shots <- all_shots %>%
    filter(teamId == home_team_id) %>%
    mutate(goal_colour = if_else(eventType == "Goal", home_team_colours, tertiary_colour)) %>%
    ggplot(aes(x = x, y = y, colour = goal_colour)) +
    annotate_pitch(colour = "#FFFFFF", fill = canvas_colour, dimensions = pitch_international) +
    coord_flip(xlim = c(55, 120),
               ylim = c(-12, 105)) +
    geom_point(size = 3) +
    scale_color_identity() +
    theme_minimal() +
    theme(panel.background = element_blank(),
          plot.background = element_rect(fill = canvas_colour, color = NA),
          plot.margin = margin(0, 0, 0, 0),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  
  gg_grob_home_shots <- ggplotGrob(home_shots)
  gg_image_home_shots <- image_graph(width = 600, height = 400, res = 98)
  grid::grid.draw(gg_grob_home_shots)
  dev.off()
  
  away_shots <- all_shots %>%
    filter(teamId == away_team_id) %>%
    mutate(goal_colour = if_else(eventType == "Goal", away_team_colours, tertiary_colour)) %>%
    ggplot(aes(x = x, y = y, colour = goal_colour)) +
    annotate_pitch(colour = "#FFFFFF", fill = canvas_colour, dimensions = pitch_international) +
    coord_flip(xlim = c(55, 120),
               ylim = c(-12, 105)) +
    geom_point(size = 3) +
    scale_color_identity() +
    theme_minimal() +
    theme(panel.background = element_blank(),
          plot.background = element_rect(fill = canvas_colour, color = NA),
          plot.margin = margin(0, 0, 0, 0),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  gg_grob_away_shots <- ggplotGrob(away_shots)
  gg_image_away_shots <- image_graph(width = 600, height = 400, res = 98)
  grid::grid.draw(gg_grob_away_shots)
  dev.off()
  
  
}else{}


#Top Players
if(nrow(home_team_top_players) > 0){
  home_top_players <- home_team_top_players %>%
    select(name, positionLabel, playerRating, manOfTheMatch)
  
  home_top_players$name$firstName <- NULL
  home_top_players$name$lastName <- NULL
  
  home_top_players$manOfTheMatch <- if_else(home_top_players$manOfTheMatch == TRUE, 
                                            '<img src="https://png.pngtree.com/png-clipart/20221231/original/pngtree-golden-soccer-ball-png-image_8836180.png" height="30"/>', 
                                            "")
  
  home_player_visual_temp <- kable(home_top_players, escape = FALSE, format = "html", align = 'c', col.names = NULL, booktabs = T) %>%
    kable_styling(bootstrap_options = c("striped"), position = "center", full_width = F) %>%
    # Row Specific
    row_spec(1:nrow(home_top_players), background = canvas_colour, color = tertiary_colour, align = "center") %>%
    kable_styling()
  
  # Save the table as an HTML file
  home_player_rating_html_file <- "home_player_rating.html"
  save_kable(home_player_visual_temp, home_player_rating_html_file)
  
  # Convert the HTML file to an image
  home_player_rating_html_img_file <- "home_player_rating_html.png"
  webshot(home_player_rating_html_file, file = home_player_rating_html_img_file, selector = "table", zoom = 1.5)
  
  # Read the image with magick
  home_player_rating_card_img <- image_read(home_player_rating_html_img_file)
}else{}

if(nrow(away_team_top_players) > 0){
  away_team_top_players <- away_team_top_players %>%
    select(name, positionLabel, playerRating, manOfTheMatch)
  
  away_team_top_players$name$firstName <- NULL
  away_team_top_players$name$lastName <- NULL
  
  away_team_top_players$manOfTheMatch <- if_else(away_team_top_players$manOfTheMatch == TRUE, 
                                                 '<img src="https://png.pngtree.com/png-clipart/20221231/original/pngtree-golden-soccer-ball-png-image_8836180.png" height="30"/>', 
                                                 "")
  
  away_player_visual_temp <- kable(away_team_top_players, escape = FALSE, format = "html", align = 'c', col.names = NULL, booktabs = T) %>%
    kable_styling(bootstrap_options = c("striped"), position = "center", full_width = F) %>%
    # Row Specific
    row_spec(1:nrow(away_team_top_players), background = canvas_colour, color = tertiary_colour, align = "center") %>%
    kable_styling()
  
  # Save the table as an HTML file
  away_player_rating_html_file <- "away_player_rating.html"
  save_kable(away_player_visual_temp, away_player_rating_html_file)
  
  # Convert the HTML file to an image
  away_player_rating_html_img_file <- "away_player_rating_html.png"
  webshot(away_player_rating_html_file, file = away_player_rating_html_img_file, selector = "table", zoom = 1.5)
  
  # Read the image with magick
  away_player_rating_card_img <- image_read(away_player_rating_html_img_file)
}else{}


## xG Map
if(nrow(all_shots) > 0){
  xg <- rbind(
    all_shots %>%
      filter(teamId == home_team_id) %>%
      mutate(full_minutes = if_else(is.na(minAdded), min, min + minAdded)) %>%
      select(teamId, eventType, full_minutes, expectedGoals) %>%
      arrange(full_minutes) %>%
      mutate(cumsum_xg = cumsum(expectedGoals))
    ,
    all_shots %>%
      filter(teamId == away_team_id) %>%
      mutate(full_minutes = if_else(is.na(minAdded), min, min + minAdded)) %>%
      select(teamId, eventType, full_minutes, expectedGoals) %>%
      arrange(full_minutes) %>%
      mutate(cumsum_xg = cumsum(expectedGoals))
  )
  
  
  max_minute <- max(xg$full_minutes)
  
  xg <- xg %>%
    group_by(teamId) %>%
    complete(full_minutes = 0:max_minute) %>%
    fill(cumsum_xg, .direction = "down") %>%
    replace_na(list(cumsum_xg = 0)) %>%
    mutate(team_colour = if_else(teamId == home_team_id, home_team_colours, away_team_colours))
  
  xg_plotting <- xg %>%
    ggplot(aes(x = full_minutes, y = cumsum_xg, group = teamId, colour = team_colour)) + geom_line(size = 2) +
    scale_color_identity() +
    theme_minimal() +
    theme(panel.background = element_rect(fill = canvas_colour, color = NA),
          plot.background = element_rect(fill = canvas_colour, color = NA),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(colour = main_text_colour, size = 21),
          axis.text.x = element_blank(),
          panel.grid.major = element_line(linetype = "dashed"),
          panel.grid.minor = element_line(linetype = "dashed"),
          legend.position = "none") 
  
  gg_grob_xg <- ggplotGrob(xg_plotting)
  gg_image_xg <- image_graph(width = 2000, height = 400, res = 98)
  grid::grid.draw(gg_grob_xg)
  dev.off()
}

################################################################################################################################

## Creating Canvas
final_image <- image_blank(width = 2000, height = 2000, color = canvas_colour)


final_image <- image_annotate(final_image, paste(league_name, "|", stadium$name, ",", stadium$city, "|", "Referee:", referee$text, "|", match_time_utc),
                              gravity = "north", location = "+0+20", weight = 100, size = 30, color = main_text_colour)

final_image <- image_annotate(final_image, final_score, gravity = "north", location = "+0+120", weight = 900, size = 125, color = main_text_colour)


# Logo
home_team_logo_img <- image_read(home_team_logo)
home_team_logo_img <- image_scale(home_team_logo_img, "250x250")
away_team_logo_img <- image_read(away_team_logo)
away_team_logo_img <- image_scale(away_team_logo_img, "250x250")

final_image <- image_composite(final_image, home_team_logo_img, gravity = "north", offset = "-300+100")
final_image <- image_composite(final_image, away_team_logo_img, gravity = "north", offset = "+300+100")

# Goals
if(nrow(home_team_goals) > 0){
  final_image <- image_composite(final_image, soccer_ball_logo, gravity = "north", offset = "+0+376")
  final_image <- image_composite(final_image, home_goals_img, gravity = "north", offset = "-200+376")
}else{} 

if(nrow(away_team_goals) > 0){
  final_image <- image_composite(final_image, soccer_ball_logo, gravity = "north", offset = "+0+376")
  final_image <- image_composite(final_image, away_goals_img, gravity = "north", offset = "+200+376")
}else{}

# Red Card
if(nrow(redCardHistory) > 0 && nrow(home_red_cards) > 0){
  final_image <- image_composite(final_image, red_card_logo, gravity = "north", offset = "-970+120")
  final_image <- image_composite(final_image, home_red_card_img, gravity = "north", offset = "-810+120")
}else{} 

if(nrow(redCardHistory) > 0 && nrow(away_red_cards) > 0){
  final_image <- image_composite(final_image, red_card_logo, gravity = "north", offset = "+970+120")
  final_image <- image_composite(final_image, away_red_card_img, gravity = "north", offset = "+810+120")
}else{}

# Yellow Card

if(nrow(match_events_cards) > 0 && nrow(home_yellow_card) > 0){
  final_image <- image_composite(final_image, yellow_card_logo, gravity = "north", offset = "-970+250")
  final_image <- image_composite(final_image, home_yellow_card_img, gravity = "north", offset = "-810+250")
}else{} 

if(nrow(match_events_cards) > 0 && nrow(away_yellow_card) > 0){
  final_image <- image_composite(final_image, yellow_card_logo, gravity = "north", offset = "+970+250")
  final_image <- image_composite(final_image, away_yellow_card_img, gravity = "north", offset = "+810+250")
}else{}

# Momentum Chart
if(length(content[["content"]][["matchFacts"]][["momentum"]][["main"]]) != 0){
  final_image <- image_composite(final_image, gg_image, gravity = "north", offset = "-0+1700")
}

# Expected Goal (xG)
colnames(match_stats_showcase)[2] <- "Stat"
home_xg <- match_stats_showcase %>%
  filter(Stat == "Expected goals (xG)") %>%
  select(home_team)
home_xg <- home_xg[1, home_team]
away_xg <- match_stats_showcase %>%
  filter(Stat == "Expected goals (xG)") %>%
  select(away_team)
away_xg <- away_xg[1, away_team]

final_image <- image_annotate(final_image, paste0(home_xg, " - ", away_xg), gravity = "north", location = "+0+100", weight = 500, size = 40, color = main_text_colour)

# Match Stats
final_image <- image_composite(final_image, match_stats_card_img, gravity = "north", offset = "-0+600")

# Match Reason
final_image <- image_annotate(final_image, reason_long, gravity = "north", location = "-0+290", weight = 500, size = 40, color = main_text_colour)


# Team Form
if(nrow(home_team_form) > 0){
  final_image <- image_composite(final_image, gg_image_home_form, gravity = "north", offset = "-475+100")
}else{}
if(nrow(away_team_form) > 0){
  final_image <- image_composite(final_image, gg_image_away_form, gravity = "north", offset = "+475+100")
}else{}


## Shot Map
if(nrow(all_shots) > 0){
  final_image <- image_composite(final_image, gg_image_home_shots, gravity = "north", offset = "-600+500")
  final_image <- image_composite(final_image, gg_image_away_shots, gravity = "north", offset = "+700+500")
  
  # xG Plotting
  final_image <- image_composite(final_image, gg_image_xg, gravity = "north", offset = "-0+1300")
}else{}


## Top Players
if(nrow(home_top_players) > 0){
  final_image <- image_composite(final_image, home_player_rating_card_img, gravity = "north", offset = "-650+950")
}else{}
if(nrow(away_team_top_players) > 0){
  final_image <- image_composite(final_image, away_player_rating_card_img, gravity = "north", offset = "+650+950")
}else{}

print(final_image)


# Delete files from the working directory
file.remove(c(
  "away_goals.png",
  "away_player_rating.png",
  "away_yellow_card.png",
  "home_goals.png",
  "home_player_rating.png",
  "home_yellow_card.png",
  "match_stats_card.png",
  "away_red_card.png",
  "home_red_card.png",
  "away_player_rating_html.png",
  "home_player_rating_html.png",
  
  "away_goals.html",
  "away_player_rating_html.html",
  "away_yellow_card.html",
  "away_red_card.html",
  "home_goals.html",
  "home_player_rating_html.html",
  "home_yellow_card.html",
  "match_stats_card_html.html",
  "home_red_card.html"
))
