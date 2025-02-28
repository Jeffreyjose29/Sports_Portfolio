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
final_image <- image_blank(width = 2000, height = 1500, color = "#1b2326")

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

final_image <- image_annotate(final_image, "Visual By @EdgeOfTheField", gravity = "north", weight = 100, size = 25, 
                              color = "#FFFFFF", location = "+830+20")


# Add Team Logo
final_image <- image_composite(final_image, innings_1_logo, gravity = "north", offset = "-100+290")
final_image <- image_composite(final_image, innings_2_logo, gravity = "north", offset = "+100+290")


# Scores
final_image <- image_annotate(final_image, innings_1_score, gravity = "north", weight = 700, size = 70, 
                              color = "#FFFFFF", location = "-300+300")
final_image <- image_annotate(final_image, innings_2_score, gravity = "north", weight = 700, size = 70, 
                              color = "#FFFFFF", location = "+300+300")

# Overs & RR
final_image <- image_annotate(final_image, innings_1_over, gravity = "north", weight = 700, size = 30, 
                              color = "#FFFFFF", location = "-300+390")
final_image <- image_annotate(final_image, innings_2_over, gravity = "north", weight = 700, size = 30, 
                              color = "#FFFFFF", location = "+300+390")


# Manhattan
final_image <- image_annotate(final_image, paste0("Manhattan: ", match$team1), gravity = "north", weight = 200, size = 25, 
                              color = "#FFFFFF", location = "-0+470")
final_image <- image_composite(final_image, gg_image_manhattan_innings1, gravity = "north", offset = "-0+500")
final_image <- image_annotate(final_image, paste0("Manhattan: ", match$team2), gravity = "north", weight = 200, size = 25, 
                              color = "#FFFFFF", location = "-0+1070")
final_image <- image_composite(final_image, gg_image_manhattan_innings2, gravity = "north", offset = "+0+1100")

final_image
