## Package names
packages <- c("dplyr", "readxl", "purrr", "sjmisc", "magick", "httr", "jsonlite", "kableExtra", "webshot", "ggplot2",
              "stringr", "cricketdata", "lubridate")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))


#############################################################
image_file_location <- "C:/Users/jeffr/OneDrive/Desktop/Github Activities/Sports_Portfolio/Cricket/Logos/Comps/"

comp_logo <- image_read(
  paste0(image_file_location, "Champions_Trophy_2025.png")) %>%
  image_resize("200x200")

team_logo_file_locations <- "C:/Users/jeffr/OneDrive/Desktop/Github Activities/Sports_Portfolio/Cricket/Logos/Intl/"

innings_1_logo <- image_read(
  paste0(team_logo_file_locations, "South_Africa.png")) %>%
  image_resize("150x150")

innings_2_logo <- image_read(
  paste0(team_logo_file_locations, "Pakistan.png")) %>%
  image_resize("150x150")

innings_1_colour <- "#f4ba0a"
innings_2_colour <- "#a8821d"

#############################################################


# 1. Get the match high-level information
match <- fetch_cricsheet(type = "match", gender = "male", competition = "odis") 
match <- match %>%
  mutate(match_id = sub(".*\\/", "", match_id)) %>%
  filter(match_id == "1442222")


bbb <- fetch_cricsheet(type = "bbb", gender = "male", competition = "odis")
bbb <- bbb %>%
  filter(match_id == "1442222")

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
match_info_potm <- paste("Player Of The Match:", match$player_of_match)


# Out. 2 - Score and Overs (RR)
innings_1_score <- paste0(innings_1$Runs, "-", innings_1$Wickets)
innings_2_score <- paste0(innings_2$Runs, "-", innings_2$Wickets)
innings_1_over <- paste0(innings_1$Overs, " (RR: ", round(innings_1$RunRate, 2), ")")
innings_2_over <- paste0(innings_2$Overs, " (RR: ", round(innings_2$RunRate, 2), ")")


# Out. 3 - Create Worm Graph
scoring_worm <- bbb %>%
  group_by(innings, ball) %>%
  summarise(
    Runs = sum(runs_off_bat, na.rm = TRUE) + 
      sum(wides, na.rm = TRUE) + 
      sum(noballs, na.rm = TRUE) + 
      sum(byes, na.rm = TRUE) + 
      sum(legbyes, na.rm = TRUE) + 
      sum(penalty, na.rm = TRUE),
    Wickets = sum(wicket_type != "" & !is.na(wicket_type)), 
    .groups = "drop"
  ) %>%
  rename("Team" = innings, "Ball" = ball) %>%
  group_by(Team) %>%
  mutate(cumsumRuns = cumsum(Runs)) %>%
  ggplot(aes(x = Ball, y = cumsumRuns, group = as.factor(Team), colour = as.factor(Team))) + 
  
  # Line with increased thickness
  geom_line(linewidth = 1.2) + 
  
  # Add points where wickets fell
  geom_point(data = . %>% filter(Wickets > 0), aes(x = Ball, y = cumsumRuns), size = 4, shape = 21, fill = "white", stroke = 1.2) +
  
  scale_colour_manual(values = c("1" = innings_1_colour, "2" = innings_2_colour)) +
  labs(x = "Over", y = "Runs") +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1f4357", color = NA),
    plot.background = element_rect(fill = "#1f4357", color = NA),
    panel.grid = element_blank(),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.position = "none"
  )


gg_grob_scoring_worm <- ggplotGrob(scoring_worm)
gg_image_scoring_worm <- image_graph(width = 730, height = 400, res = 98)
grid::grid.draw(gg_grob_scoring_worm)
dev.off()

# Out. 4 - Manhattan
manhattan <- bbb %>%
  mutate(Over = floor(ball) + 1) %>%
  group_by(innings, Over) %>%
  summarise(Runs = sum(runs_off_bat, na.rm = TRUE) + 
              sum(wides, na.rm = TRUE) + 
              sum(noballs, na.rm = TRUE) + 
              sum(byes, na.rm = TRUE) + 
              sum(legbyes, na.rm = TRUE) + 
              sum(penalty, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Over, y = Runs, group = as.factor(innings), fill = as.factor(innings))) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("1" = innings_1_colour, "2" = innings_2_colour)) +
  labs(x = "Over", y = "Runs") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1f4357", color = NA),  # Removes panel background
    plot.background = element_rect(fill = "#1f4357", color = NA),   # Removes plot background
    panel.grid = element_blank(),
    axis.title = element_text(color = "white"),  # Makes axis titles white
    axis.text = element_text(color = "white"),   # Makes axis numbers white
    legend.position = "none"  # Removes legend
  )

gg_grob_manhattan <- ggplotGrob(manhattan)
gg_image_manhattan <- image_graph(width = 730, height = 400, res = 98)
grid::grid.draw(gg_grob_manhattan)
dev.off()


## Out. 3 - Score Cards
top_scores <- bbb %>%
  group_by(innings, striker) %>%
  summarise(Runs = sum(runs_off_bat, na.rm = TRUE),
            Balls = n(),  # Counts total rows (balls faced)
            Byes = sum(!is.na(byes)),  # Counts non-NA rows in Byes
            LegByes = sum(!is.na(legbyes)),
            Wides = sum(!is.na(wides)),
            `4s` = sum(runs_off_bat == 4, na.rm = TRUE),
            `6s` = sum(runs_off_bat == 6, na.rm = TRUE)) %>%
  mutate(`Balls Faced` = Balls - Wides) %>%
  select(- c(Balls, Byes, LegByes, Wides)) 
