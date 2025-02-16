## Package names
packages <- c("dplyr", "readxl", "purrr", "sjmisc", "plyr", "magick", "httr", "jsonlite", "kableExtra", "webshot", "ggplot2",
              "stringr", "cricketdata", "lubridate")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# 1. Get the match high-level information
match <- fetch_cricsheet(type = "match", gender = "male", competition = "bbl")
match <- match %>%
  filter(date == "2017/01/28")


bbb <- fetch_cricsheet(type = "bbb", gender = "male", competition = "bbl")
bbb <- bbb %>%
  filter(match_id == "1023649")

# 2. Get the score
innings_1 <- bbb %>%
  filter(innings == 1) %>%
  group_by(batting_team) %>%
  summarise(
    Team = max(batting_team),
    Overs = max(ball),
    Runs = sum(runs_off_bat, na.rm = TRUE) + 
      sum(wides, na.rm = TRUE) + 
      sum(noballs, na.rm = TRUE) + 
      sum(byes, na.rm = TRUE) + 
      sum(legbyes, na.rm = TRUE) + 
      sum(penalty, na.rm = TRUE),
    Wickets = sum(wicket_type != "" & !is.na(wicket_type))
  ) %>%
  mutate(RunRate = max(Runs) / max(Overs))

innings_2 <- bbb %>%
  filter(innings == 2) %>%
  group_by(batting_team) %>%
  summarise(
    Team = max(batting_team),
    Overs = max(ball),
    Runs = sum(runs_off_bat, na.rm = TRUE) + 
      sum(wides, na.rm = TRUE) + 
      sum(noballs, na.rm = TRUE) + 
      sum(byes, na.rm = TRUE) + 
      sum(legbyes, na.rm = TRUE) + 
      sum(penalty, na.rm = TRUE),
    Wickets = sum(wicket_type != "" & !is.na(wicket_type))
  ) %>%
  mutate(RunRate = max(Runs) / max(Overs))

# Out. 1 - Create Match Info String
match_info_teams <- paste0(match$team1, " vs. ", match$team2)
match_info_demo <- paste(match$event, "|", match$date, "|", paste0(match$venue, ","), match$city)
match_info_umpires <- paste("Umpires:", paste0(match$umpire1, ", ", match$umpire2))


# Out. 2 - Score and Overs (RR)
innings_1_score <- paste0(innings_1$Runs, "-", innings_1$Wickets)
innings_2_score <- paste0(innings_2$Runs, "-", innings_2$Wickets)
innings_1_over <- paste0(innings_1$Overs, " (RR: ", round(innings_1$RunRate, 2), ")")
innings_2_over <- paste0(innings_2$Overs, " (RR: ", round(innings_2$RunRate, 2), ")")



