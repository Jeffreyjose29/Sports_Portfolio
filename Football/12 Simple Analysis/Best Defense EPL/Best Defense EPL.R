library(worldfootballR)
library(dplyr)
library(ggplot2)
library(Cairo)


setwd("C:/Users/jeffr/OneDrive/Desktop/Github Activities/Sports_Portfolio/Football/12 Simple Analysis/Best Defense EPL/Figures")


start_season <- 2023
current_season <- 2024

leagues <- c("EPL") #, "La liga", "Bundesliga", "Serie A", "Ligue 1"

team_stats_final <- data.frame()
# Get 5 league teams
for(i in 1:length(leagues)){
  print(leagues[i])
  teams <- understat_available_teams(leagues[i])
  
  for(j in 1:length(teams)){
    print(teams[j])
    team_stats <- understat_team_stats_breakdown(paste0("https://understat.com/team/", gsub(" ", "_", teams[j]), "/2024"))
    
    team_stats_final <- rbind(team_stats_final, team_stats)
  }
}


team_stats_final <- team_stats_final %>%
  filter(stat_name == "OpenPlay" | stat_name == "FromCorner" | stat_name == "SetPiece" | stat_name == "DirectFreekick")

team_stats_final <- team_stats_final %>%
  filter(season_start_year == 2024) %>%
  dplyr::group_by(team_name, season_start_year) %>%
  summarise(shots = sum(shots, na.rm = TRUE), 
            goals = sum(goals, na.rm = TRUE),
            xG = sum(xG, na.rm = TRUE),
            shots_against = sum(against.shots, na.rm = TRUE),
            goals_against = sum(against.goals, na.rm = TRUE),
            xGA = sum(against.xG, na.rm = TRUE))


team_stats_final$scoring_efficiency <- team_stats_final$goals / team_stats_final$shots
team_stats_final$conceding_rate <- team_stats_final$goals_against / team_stats_final$shots_against

# Define the teams to highlight
highlight_teams <- c("Arsenal", "Liverpool", "Manchester City", "Nottingham Forest")

# Add a new column to flag the teams to highlight
team_stats_final <- team_stats_final %>%
  mutate(highlight = ifelse(team_name %in% highlight_teams, "Highlighted", "Other"))

# Plot with conditional coloring
team_stats_final %>%
  ggplot(aes(x = xGA, y = reorder(team_name, -xGA), fill = highlight)) +  # Conditional fill
  geom_bar(stat = "identity") +  # Bars colored based on 'highlight' column
  scale_fill_manual(values = c("Highlighted" = "#f1cb5c", "Other" = "#FFFFFF")) +  # Custom colors
  labs(
    title = "WHAT IS THE BEST DEFENSE SO FAR IN THE EPL?",
    subtitle = "Lowest expected goals against from the current season (2024/25) | Best xGA team from 2023/24 season are highlighted",
    caption = "Data Source: understat.com"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#121212", color = NA),
    plot.background = element_rect(fill = "#121212", color = NA),
    axis.text.y = element_text(colour = "#FFFFFF", size = 15),
    axis.title.y = element_blank(),
    axis.text.x = element_text(colour = "#FFFFFF", size = 15),
    axis.text = element_text(color = "white"),         # White axis text for readability
    axis.title = element_text(color = "white", face = "bold"),        # White axis titles
    plot.title = element_text(color = "white", size = 30, face = "bold"),  # Bold title
    plot.subtitle = element_text(color = "white", size = 15),  # Subtitle size and color
    plot.caption = element_text(color = "white", size = 12),  # Caption size and color
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"                           # Remove legend
  )
