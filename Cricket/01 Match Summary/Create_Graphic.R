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
final_image <- image_blank(width = 1500, height = 1500, color = "#1f4357")

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
final_image <- image_annotate(final_image, innings_1_score, gravity = "north", weight = 700, size = 100, 
                              color = "#FFFFFF", location = "-150+280")
final_image <- image_annotate(final_image, innings_2_score, gravity = "north", weight = 700, size = 100, 
                              color = "#FFFFFF", location = "+150+280")

# Overs & RR
final_image <- image_annotate(final_image, innings_1_over, gravity = "north", weight = 700, size = 30, 
                              color = "#FFFFFF", location = "-150+400")
final_image <- image_annotate(final_image, innings_2_over, gravity = "north", weight = 700, size = 30, 
                              color = "#FFFFFF", location = "+150+400")

# Add Team Logo
final_image <- image_composite(final_image, innings_1_logo, gravity = "north", offset = "-375+290")
final_image <- image_composite(final_image, innings_2_logo, gravity = "north", offset = "+375+290")


# Add Manhattan
final_image <- image_annotate(final_image, "Manhattan", gravity = "north", weight = 200, size = 20, 
                              color = "#FFFFFF", location = "-350+1075")
final_image <- image_composite(final_image, gg_image_manhattan, gravity = "west", offset = "+0+550")


# Scoring Worm
final_image <- image_annotate(final_image, "Scoring Worm", gravity = "north", weight = 200, size = 20, 
                              color = "#FFFFFF", location = "+350+1075")
final_image <- image_composite(final_image, gg_image_scoring_worm, gravity = "east", offset = "+0+550")



final_image
