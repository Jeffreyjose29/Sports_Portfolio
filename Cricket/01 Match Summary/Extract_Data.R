## Package names
packages <- c("dplyr", "readxl", "purrr", "sjmisc", "magick", "httr", "jsonlite", "kableExtra", "webshot", "ggplot2",
              "stringr", "cricketdata", "lubridate", "howzatR")

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
  paste0(team_logo_file_locations, "England.png")) %>%
  image_resize("150x150")

innings_2_logo <- image_read(
  paste0(team_logo_file_locations, "Australia.png")) %>%
  image_resize("150x150")

innings_1_colour <- "#029ac3"
innings_2_colour <- "#fff32b"

#############################################################

match_id_ <- "1466417"

# 1. Get the match high-level information
match <- fetch_cricsheet(type = "match", gender = "male", competition = "odis") 
match <- match %>%
  mutate(match_id = sub(".*\\/", "", match_id)) %>%
  filter(match_id == match_id_)


bbb <- fetch_cricsheet(type = "bbb", gender = "male", competition = "odis")
bbb <- bbb %>%
  filter(match_id == match_id_)

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
    panel.background = element_rect(fill = "#1b2326", color = NA),
    plot.background = element_rect(fill = "#1b2326", color = NA),
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
    panel.background = element_rect(fill = "#1b2326", color = NA),  # Removes panel background
    plot.background = element_rect(fill = "#1b2326", color = NA),   # Removes plot background
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
  select(- c(Balls, Byes, LegByes, Wides)) %>%
  mutate(`SR` = round((Runs/`Balls Faced`)* 100, 2)) %>%
  rename("Batting" = striker, "R" = Runs, "B" = `Balls Faced`)

innings_1_scorecard <- top_scores %>%
  filter(innings == 1) %>%
  ungroup() %>% 
  mutate(`% R` = round((`R` / innings_1$Runs)*100, 2)) %>%
  select(-innings) %>%
  select(`Batting`, `R`, `B`, `4s`, `6s`, `SR`, `% R`) %>%
  arrange(desc(`R`))


innings_2_scorecard <- top_scores %>%
  filter(innings == 2) %>%
  ungroup() %>%  
  mutate(`% R` = round((`R` / innings_2$Runs) * 100, 2)) %>%
  select(-innings) %>%
  select(`Batting`, `R`, `B`, `4s`, `6s`, `SR`, `% R`) %>%
  arrange(desc(`R`))


# Kable Table Scorecards
innings_1_card <- kable(innings_1_scorecard, escape = FALSE, format = "html", align = 'c', booktabs = TRUE) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    position = "center",
    full_width = FALSE,
    html_font = "Arial"
  ) %>%
  row_spec(0, background = innings_1_colour, extra_css = "border: none;", bold = TRUE, color = "#FFFFFF") %>%
  row_spec(1:nrow(innings_1_scorecard), background = "#1b2326", color = "#FFFFFF", extra_css = "border: none;") %>%
  column_spec(1, width = "9em", include_thead = TRUE) %>%
  column_spec(2, width = "4em", include_thead = TRUE) %>%
  column_spec(3, width = "4em", include_thead = TRUE) %>%
  column_spec(4, width = "4em", include_thead = TRUE) %>%
  column_spec(5, width = "4em", include_thead = TRUE) %>%
  column_spec(6, width = "4em", include_thead = TRUE) %>%
  column_spec(7, width = "4em", include_thead = TRUE) 


# Save the table as an HTML file
innings1_html <- "innings1_html.html"
save_kable(innings_1_card, innings1_html)

# Convert the HTML file to an image
innings1_img_file <- "innings1_card.png"
webshot(innings1_html, file = innings1_img_file, selector = "table", zoom = 1.0)

# Read the image with magick
innings1_img <- image_read(innings1_img_file)



innings_2_card <- kable(innings_2_scorecard, escape = FALSE, format = "html", align = 'c', booktabs = TRUE) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    position = "center",
    full_width = FALSE,
    html_font = "Arial"
  ) %>%
  row_spec(0, background = innings_2_colour, extra_css = "border: none;", bold = TRUE, color = "#FFFFFF") %>%
  row_spec(1:nrow(innings_2_scorecard), background = "#1b2326", color = "#FFFFFF", extra_css = "border: none;") %>%
  column_spec(1, width = "9em", include_thead = TRUE) %>%
  column_spec(2, width = "4em", include_thead = TRUE) %>%
  column_spec(3, width = "4em", include_thead = TRUE) %>%
  column_spec(4, width = "4em", include_thead = TRUE) %>%
  column_spec(5, width = "4em", include_thead = TRUE) %>%
  column_spec(6, width = "4em", include_thead = TRUE) %>%
  column_spec(7, width = "4em", include_thead = TRUE) 


innings2_html <- "innings2_html.html"
save_kable(innings_2_card, innings2_html)

# Convert the HTML file to an image
innings2_img_file <- "innings2_card.png"
webshot(innings2_html, file = innings2_img_file, selector = "table", zoom = 1.0)

# Read the image with magick
innings2_img <- image_read(innings2_img_file)


### Bowling Card Innings 1
bowling_scorecard <- bbb %>%
  group_by(innings, bowler) %>%
  summarise(RunsConceded = sum(runs_off_bat, na.rm = TRUE),
            Balls = n(),  # Counts total rows (balls faced)
            Byes = sum(!is.na(byes)),  # Counts non-NA rows in Byes
            LegByes = sum(!is.na(legbyes)),
            Wides = sum(!is.na(wides)),
            `W` = sum(!is.na(wicket_type) & !(wicket_type %in% c('run out', '')))) %>%
  mutate(`R` = RunsConceded + Wides,
         `O` = balls_to_overs(Balls-Wides),
         `Econ` = round(`R` / `O`, 2)) %>%
  select(- c(RunsConceded, Balls, Byes, LegByes, Wides)) %>%
  rename("Bowling" = bowler) %>%
  select(`Bowling`, `O`, `R`, `W`, `Econ`) %>%
  arrange(desc(`W`))


bowling_innings_1 <- bowling_scorecard %>%
  filter(innings == 1) %>%
  ungroup() %>%
  select(-c(innings))


bowling_innings_1_card <- kable(bowling_innings_1, escape = FALSE, format = "html", align = 'c', booktabs = TRUE) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    position = "center",
    full_width = FALSE,
    html_font = "Arial"
  ) %>%
  row_spec(0, background = innings_2_colour, extra_css = "border: none;", bold = TRUE, color = "#FFFFFF") %>%
  row_spec(1:nrow(bowling_innings_1), background = "#1b2326", color = "#FFFFFF", extra_css = "border: none;") %>%
  column_spec(1, width = "9em", include_thead = TRUE) %>%
  column_spec(2, width = "4em", include_thead = TRUE) %>%
  column_spec(3, width = "4em", include_thead = TRUE) %>%
  column_spec(4, width = "4em", include_thead = TRUE) %>%
  column_spec(5, width = "4em", include_thead = TRUE) 


# Save the table as an HTML file
innings1_html <- "innings1_html.html"
save_kable(bowling_innings_1_card, innings1_html)

# Convert the HTML file to an image
innings1_img_file <- "innings1_card.png"
webshot(innings1_html, file = innings1_img_file, selector = "table", zoom = 1.0)

# Read the image with magick
bowling_innings1_img <- image_read(innings1_img_file)




bowling_innings_2 <- bowling_scorecard %>%
  filter(innings == 2) %>%
  ungroup() %>%
  select(-c(innings))


bowling_innings_2_card <- kable(bowling_innings_2, escape = FALSE, format = "html", align = 'c', booktabs = TRUE) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    position = "center",
    full_width = FALSE,
    html_font = "Arial"
  ) %>%
  row_spec(0, background = innings_1_colour, extra_css = "border: none;", bold = TRUE, color = "#FFFFFF") %>%
  row_spec(1:nrow(bowling_innings_2), background = "#1b2326", color = "#FFFFFF", extra_css = "border: none;") %>%
  column_spec(1, width = "9em", include_thead = TRUE) %>%
  column_spec(2, width = "4em", include_thead = TRUE) %>%
  column_spec(3, width = "4em", include_thead = TRUE) %>%
  column_spec(4, width = "4em", include_thead = TRUE) %>%
  column_spec(5, width = "4em", include_thead = TRUE) 


innings2_html <- "innings2_html.html"
save_kable(bowling_innings_2_card, innings2_html)

# Convert the HTML file to an image
innings2_img_file <- "innings2_card.png"
webshot(innings2_html, file = innings2_img_file, selector = "table", zoom = 1.0)

# Read the image with magick
bowling_innings2_img <- image_read(innings2_img_file)



# Out. 4. Run-Rate
run_rate <- bbb %>%
  mutate(Over = floor(ball) + 1) %>%
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
  labs(x = "Overs", y = "Run Rate") +
  
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1b2326", color = NA),
    plot.background = element_rect(fill = "#1b2326", color = NA),
    panel.grid.major.x = element_blank(),  # Remove major vertical gridlines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical gridlines
    panel.grid.major.y = element_line(color = "white"),  # Keep horizontal gridlines
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal gridlines (optional)
    axis.title = element_text(color = "white"),  # Bigger axis title with grey color
    axis.text = element_text(color = "white"),  # Bigger axis text with grey color
    legend.position = "none"
  )

gg_grob_runrate <- ggplotGrob(run_rate)
gg_image_runrate <- image_graph(width = 730, height = 400, res = 98)
grid::grid.draw(gg_grob_runrate)
dev.off()


#Out 5. Winner Text
match1 <- unlist(match)
match_winner_text <- if_else(is.na(match1["winner_runs"]), paste0(match1["winner"], " won by ", match1["winner_wickets"], " wickets"), paste0(match1["winner"], " won by ", match1["winner_runs"], " runs"))
