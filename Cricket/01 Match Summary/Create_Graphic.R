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

#############################################################
image_file_location <- "C:/Users/jeffr/OneDrive/Desktop/Github Activities/Sports_Portfolio/Cricket/Logos/Comps/"

comp_logo <- image_read(
  paste0(image_file_location, "Champions_Trophy_2025.png")) %>%
  image_resize("200x200")

team_logo_file_locations <- "C:/Users/jeffr/OneDrive/Desktop/Github Activities/Sports_Portfolio/Cricket/Logos/Intl/"

innings_1_logo <- image_read(
  paste0(team_logo_file_locations, "India.png")) %>%
  image_resize("150x150")

innings_2_logo <- image_read(
  paste0(team_logo_file_locations, "South_Africa.png")) %>%
  image_resize("150x150")

#############################################################

# Create canvas
final_image <- image_blank(width = 1500, height = 1500, color = "#03a9f5")

# Add Competition Logo
final_image <- image_composite(final_image, comp_logo, gravity = "north", offset = "+0+20")

# Add Match Information
final_image <- image_annotate(final_image, match_info_teams, gravity = "north", weight = 700, size = 25, 
                              color = "#FFFFFF", location = "+0+115")
final_image <- image_annotate(final_image, match_info_demo, gravity = "north", weight = 100, size = 25, 
                              color = "#FFFFFF", location = "+0+150")
final_image <- image_annotate(final_image, match_info_umpires, gravity = "north", weight = 100, size = 25, 
                              color = "#FFFFFF", location = "+0+185")


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

final_image
