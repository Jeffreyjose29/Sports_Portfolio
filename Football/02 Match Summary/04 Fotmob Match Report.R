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
