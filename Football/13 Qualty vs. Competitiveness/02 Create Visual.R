## Package names
packages <- c("dplyr", "readxl", "purrr", "sjmisc", "worldfootballR", "plyr", "magick", "httr", "jsonlite", "kableExtra", "webshot", "ggplot2",
              "stringr", "StatsBombR", "ggsoccer", "reactable", "knitr", "lubridate", "kableExtra", "formattable", "htmltools", "webshot")


## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Creating The Visual


matches_viz <- matches %>%
  select(matchTime, HomeTeamLogo, teamsPlaying, AwayTeamLogo, QualityNormalized, draw_odds_perc, xScore) %>%
  arrange(desc(xScore)) %>%
  mutate(QualityNormalized = round(QualityNormalized, 2),
         draw_odds_perc = round(draw_odds_perc, 2), 
         xScore = round(xScore, 2),
         matchTime = format(ymd_hms(matchTime), "%Y-%m-%d (%H:00)")) %>%
  dplyr::rename("Date" = matchTime, "Home" = HomeTeamLogo, "Fixture" = teamsPlaying, "Away" = AwayTeamLogo, "xQuality" = QualityNormalized, 
                "xCompetitiveness" = draw_odds_perc, "xScore" = xScore) %>%
  head(top_n)


min_xQuality <- min(matches_viz$xQuality)
max_xQuality <- max(matches_viz$xQuality)
min_xCompetitiveness <- min(matches_viz$xCompetitiveness)
max_xCompetitiveness <- max(matches_viz$xCompetitiveness)
min_xScore <- min(matches_viz$xScore)
max_xScore <- max(matches_viz$xScore)


# Create a color palette function from white to green
gradient_colors <- colorRampPalette(c("white", "#e5bc00"))
gradient_colors_score <- colorRampPalette(c("white", "#16b281"))

# Function to get color based on value
value_to_color <- function(value, min_value, max_value) {
  if (is.na(value)) return("white")  # Handle NA values
  normalized_value <- (value - min_value) / (max_value - min_value)
  color <- gradient_colors(100)[round(normalized_value * 99) + 1]
  return(color)
}

# Function to get color based on value
value_to_color_xscore <- function(value, min_value, max_value) {
  if (is.na(value)) return("white")  # Handle NA values
  normalized_value <- (value - min_value) / (max_value - min_value)
  color <- gradient_colors_score(100)[round(normalized_value * 99) + 1]
  return(color)
}


comp <- matches_viz %>%
  mutate(xQuality = cell_spec(xQuality, 
                              background = sapply(xQuality, value_to_color, min_xQuality, max_xQuality)),
         xCompetitiveness = cell_spec(xCompetitiveness, 
                                      background = sapply(xCompetitiveness, value_to_color, min_xCompetitiveness, max_xCompetitiveness)),
         xScore = cell_spec(xScore, background = sapply(xScore, value_to_color_xscore, min_xScore, max_xScore))) %>%
  kbl(caption = "<span style='font-size:30px; color:black; font-weight:bold;'>MOST EXCITING ENGLISH PREMIER LEAGUE MATCHES (THIS WEEKEND)</span><br><span style='font-size:16px; color:gray;'>Viz By Jeffrey | Data Source: Fotmob</span>", 
      booktabs = TRUE, 
      align = 'c',
      escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center") %>%
  column_spec(1, width = "100px") %>%  # Adjust width for specific columns
  column_spec(2, width = "50px") %>%
  column_spec(3, width = "350px") %>%  # Adjust width for specific columns
  column_spec(4, width = "50px") %>%
  column_spec(5, width = "100px") %>%  # Adjust width for specific columns
  column_spec(6, width = "100px") %>%
  column_spec(7, width = "100px") %>%  # Adjust width for specific columns
  row_spec(0, bold = TRUE, background = competition_colour, color = header_colour)

# Save as html first manually

setwd("C:/Users/jeffr/OneDrive/Desktop/KBFC/Phase System")

# Capture the HTML file as an image
webshot("table2.html", file = "table2.png", vwidth = 1000, vheight = 600, zoom = 2)
