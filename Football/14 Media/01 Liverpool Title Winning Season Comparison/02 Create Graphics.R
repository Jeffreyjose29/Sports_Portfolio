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
final_image <- image_blank(width = 3500, height = 2000, color = "#1b2326")


# Titles
final_image <- image_composite(final_image, club_logo, gravity = "west", offset = "+20-870")

final_image <- image_annotate(final_image, "LIVERPOOL F.C TITLE WINNING SEASON (2019/20) VS. 2024/25", gravity = "west", weight = 900, size = 90, 
                              color = "#FFFFFF", location = "+270-900")
final_image <- image_annotate(final_image, "A 3-Game Running Total Of Scoring & Conceding Outputs In The English Premier League", gravity = "west", weight = 300, size = 50, 
                              color = "#A6A6A6", location = "+270-790")
final_image <- image_annotate(final_image, "Visual Created By @EdgeOfTheField", gravity = "west", weight = 100, size = 35, 
                              color = "#A6A6A6", location = "+270-720")

# Previous Season Title
final_image <- image_annotate(final_image, "Season 2019/20", gravity = "north", weight = 700, size = 50, 
                              color = "#FFFFFF", location = "-1540+350")
final_image <- image_annotate(final_image, season_output_text_prev_season, gravity = "west", weight = 200, size = 35, 
                              color = "#A6A6A6", location = "+20-560")


# Previous Season
final_image <- image_composite(final_image, gg_image_rolling_g_vs_ga_prev, gravity = "west", offset = "+0-160")

final_image <- image_composite(final_image, gg_image_rolling_xg_xga_prev, gravity = "north", offset = "+0+550")
final_image <- image_composite(final_image, gg_image_rolling_g_vs_xg_prev, gravity = "east", offset = "+0-160")


# Current Season Title
final_image <- image_annotate(final_image, "Season 2024/25", gravity = "north", weight = 700, size = 50, 
                              color = "#FFFFFF", location = "-1540+1200")
final_image <- image_annotate(final_image, season_output_text_2025, gravity = "west", weight = 200, size = 35, 
                              color = "#A6A6A6", location = "+20+285")

# Current Season
final_image <- image_composite(final_image, gg_image_rolling_gf_vs_ga_curr, gravity = "west", offset = "+0+685")
final_image <- image_composite(final_image, gg_image_rolling_xg_xga_curr, gravity = "north", offset = "+0+1390")
final_image <- image_composite(final_image, gg_image_rolling_g_vs_xg_curr, gravity = "east", offset = "+0+685")


final_image
