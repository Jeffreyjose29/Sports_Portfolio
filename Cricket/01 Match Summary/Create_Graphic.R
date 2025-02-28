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


# Create canvas
final_image <- image_blank(width = 2230, height = 1500, color = "#1b2326")

# Add Competition Logo
final_image <- image_composite(final_image, comp_logo, gravity = "north", offset = "+0+20")

# Add Match Information
final_image <- image_annotate(final_image, match_info_teams, gravity = "north", weight = 700, size = 25, 
                              color = "#FFFFFF", location = "+0+115")
final_image <- image_annotate(final_image, match_info_demo, gravity = "north", weight = 100, size = 25, 
                              color = "#FFFFFF", location = "+0+150")
final_image <- image_annotate(final_image, match_info_umpires, gravity = "north", weight = 100, size = 25, 
                              color = "#FFFFFF", location = "+0+185")
final_image <- image_annotate(final_image, match_info_potm, gravity = "north", weight = 100, size = 25, 
                              color = "#FFFFFF", location = "+0+220")


# Scores
final_image <- image_annotate(final_image, innings_1_score, gravity = "north", weight = 700, size = 75, 
                              color = "#FFFFFF", location = "-150+300")
final_image <- image_annotate(final_image, innings_2_score, gravity = "north", weight = 700, size = 75, 
                              color = "#FFFFFF", location = "+150+300")

# Overs & RR
final_image <- image_annotate(final_image, innings_1_over, gravity = "north", weight = 700, size = 30, 
                              color = "#FFFFFF", location = "-150+395")
final_image <- image_annotate(final_image, innings_2_over, gravity = "north", weight = 700, size = 30, 
                              color = "#FFFFFF", location = "+150+395")

# Add Team Logo
final_image <- image_composite(final_image, innings_1_logo, gravity = "north", offset = "-375+290")
final_image <- image_composite(final_image, innings_2_logo, gravity = "north", offset = "+375+290")


# Winner Text
final_image <- image_annotate(final_image, match_winner_text, gravity = "north", weight = 300, size = 30, 
                              color = "#FFFFFF", location = "+0+480")

# Add Manhattan
final_image <- image_annotate(final_image, "Manhattan", gravity = "north", weight = 200, size = 20, 
                              color = "#FFFFFF", location = "-750+1050")
final_image <- image_composite(final_image, gg_image_manhattan, gravity = "west", offset = "+0+550")


# Scoring Worm
final_image <- image_annotate(final_image, "Scoring Worm", gravity = "north", weight = 200, size = 20, 
                              color = "#FFFFFF", location = "+0+1050")
final_image <- image_composite(final_image, gg_image_scoring_worm, gravity = "west", offset = "+750+550")


# Run Rate
final_image <- image_annotate(final_image, "Run Rate", gravity = "north", weight = 200, size = 20, 
                              color = "#FFFFFF", location = "+750+1050")
final_image <- image_composite(final_image, gg_image_runrate, gravity = "west", offset = "+1500+550")


# Batting Card
final_image <- image_composite(final_image, innings1_img, gravity = "north", offset = "-300+560")
final_image <- image_composite(final_image, innings2_img, gravity = "north", offset = "+300+560")


# Bowling Card
final_image <- image_composite(final_image, bowling_innings1_img, gravity = "north", offset = "-800+560")
final_image <- image_composite(final_image, bowling_innings2_img, gravity = "north", offset = "+800+560")

final_image


# Delete files from the working directory
file.remove(c(
  "innings1_card.png",
  "innings1_html.html",
  "innings2_card.png",
  "innings2_html.html"
))
