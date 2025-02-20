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
  paste0(team_logo_file_locations, "Bangladesh.png")) %>%
  image_resize("150x150")

innings_2_logo <- image_read(
  paste0(team_logo_file_locations, "India.png")) %>%
  image_resize("150x150")

innings_1_colour <- "#016533"
innings_2_colour <- "#04a5e5"

#############################################################


# 1. Get the match high-level information
match <- fetch_cricsheet(type = "match", gender = "male", competition = "odis") 
match <- match %>%
  mutate(match_id = sub(".*\\/", "", match_id)) %>%
  filter(match_id == "1466415")


bbb <- fetch_cricsheet(type = "bbb", gender = "male", competition = "odis")
bbb <- bbb %>%
  filter(match_id == "1466415") %>%
  mutate(Over = floor(ball) + 1)


# Out. 1. Run-Rate
run_rate <- bbb %>%
  group_by(innings, Over) %>%
  summarise(
    `RUNS` = sum(runs_off_bat, na.rm = TRUE) + 
      sum(wides, na.rm = TRUE) + 
      sum(noballs, na.rm = TRUE) + 
      sum(byes, na.rm = TRUE) + 
      sum(legbyes, na.rm = TRUE) + 
      sum(penalty, na.rm = TRUE),
    Wickets = sum(wicket_type != "" & !is.na(wicket_type)), 
    .groups = "drop"
  ) %>%
  rename("Team" = innings, "OVERS" = Over) %>%
  group_by(Team) %>%
  mutate(`TOTAL RUNS` = cumsum(`RUNS`),
         `RUN RATE INNINGS` = `RUNS` / OVERS,
         `RUN RATE` = `TOTAL RUNS` / OVERS ) %>%
  ggplot(aes(x = OVERS, y = `RUN RATE`, group = as.factor(Team), colour = as.factor(Team))) + 
  
  # Line with increased thickness
  geom_line(linewidth = 1.2) + 
  
  # Add points where wickets fell
  geom_point(data = . %>% filter(Wickets > 0), aes(x = OVERS, y = `RUN RATE`), size = 4, shape = 21, fill = "white", stroke = 1.2) +
  
  scale_colour_manual(values = c("1" = innings_1_colour, "2" = innings_2_colour)) +
  labs(x = "OVERS", y = "RUN RATE") +
  
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1b2326", color = NA),
    plot.background = element_rect(fill = "#1b2326", color = NA),
    panel.grid.major.x = element_blank(),  # Remove major vertical gridlines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical gridlines
    panel.grid.major.y = element_line(color = "grey50"),  # Keep horizontal gridlines
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal gridlines (optional)
    axis.title = element_text(size = 16, color = "grey"),  # Bigger axis title with grey color
    axis.text = element_text(size = 14, color = "grey"),  # Bigger axis text with grey color
    legend.position = "none"
  )

gg_grob_runrate <- ggplotGrob(run_rate)
gg_image_runrate <- image_graph(width = 1950, height = 1000, res = 98)
grid::grid.draw(gg_grob_runrate)
dev.off()


# Out. 2 - Create Match Info String
match_info_teams <- paste0(match$team1, " vs. ", match$team2)
match_info_demo <- paste(match$event, "|", match$date, "|", paste0(match$venue, ","), match$city)
match_info_umpires <- paste("Umpires:", paste0(match$umpire1, ", ", match$umpire2))
match_info_potm <- paste("Player Of The Match:", match$player_of_match)