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

# Run Rate Chart
final_image <- image_annotate(final_image, "RUN RATE", gravity = "north", weight = 200, size = 30, 
                              color = "#FFFFFF", location = "-900+470")
final_image <- image_composite(final_image, gg_image_runrate, gravity = "north", offset = "+0+500")



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

final_image
