## Package names
packages <- c("dplyr", "readxl", "purrr", "sjmisc", "worldfootballR", "plyr", "magick")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))


## Variables To Filter
country_code <- "GER"
gender <- "M"
season_end_year <- 2024
division <- "1st"

team <- "Leverkusen"
main_text_colour <- "#FFFFFF"
canvas_colour <- "#000000"

competition_logo <- image_read("https://schah.online/img/public/bundesliga-logo-white-bg.png")
competition_logo <- image_scale(competition_logo, "275x300!")

second_competition_logo <- image_read("https://www.sportmonks.com/wp-content/uploads/2022/06/image-2.png")
second_competition_logo <- image_scale(second_competition_logo, "300x300!")

team_logo <- image_read("https://1000logos.net/wp-content/uploads/2022/03/Bayer-04-Leverkusen-Logo.png")
team_logo <- image_scale(team_logo, "531x300!")


league_table <- get_season_team_stats(country_code, gender, season_end_year, division, c("league_table")) %>%
  mutate(Identifier = paste0(Competition_Name, Gender, Country, Season_End_Year, Squad, Team_or_Opponent)) %>%
  filter(Squad == team)
standard <- get_season_team_stats(country_code, gender, season_end_year, division, c("standard")) %>%
  mutate(Identifier = paste0(Competition_Name, Gender, Country, Season_End_Year, Squad, Team_or_Opponent)) %>%
  filter(Squad == team)
keeper <- get_season_team_stats(country_code, gender, season_end_year, division, c("keeper")) %>%
  mutate(Identifier = paste0(Competition_Name, Gender, Country, Season_End_Year, Squad, Team_or_Opponent)) %>%
  filter(Squad == team)
shooting <- get_season_team_stats(country_code, gender, season_end_year, division, c("shooting")) %>%
  mutate(Identifier = paste0(Competition_Name, Gender, Country, Season_End_Year, Squad, Team_or_Opponent)) %>%
  filter(Squad == team)
playing_time <- get_season_team_stats(country_code, gender, season_end_year, division, c("playing_time")) %>%
  mutate(Identifier = paste0(Competition_Name, Gender, Country, Season_End_Year, Squad, Team_or_Opponent)) %>%
  filter(Squad == team)
misc <- get_season_team_stats(country_code, gender, season_end_year, division, c("misc")) %>%
  mutate(Identifier = paste0(Competition_Name, Gender, Country, Season_End_Year, Squad, Team_or_Opponent)) %>%
  filter(Squad == team)


competition <- max(league_table$Competition_Name)


# Create Key Stats Card
season_overview <- paste0("W: ", max(league_table$W), " | D: ", max(league_table$D), " | L: ", max(league_table$L))

matches_played <- max(standard$Min_Playing_Time)

goals <- max(shooting$Gls_Standard)
goals_per_match <- paste0(round(as.double(max(shooting$Gls_Standard) / max(shooting$Mins_Per_90)), 2), " avg. per match")
possession <- paste0(round(max(standard$Poss), 2), "%")
goals_conceded <- max(keeper$GA)
goals_conceded_per_match <- paste0(round(max(keeper$GA90), 2), " avg. per match")
clean_sheet <- max(keeper$CS)
clean_sheet_per_match <- paste0(round(as.double(max(keeper$CS) / max(shooting$Mins_Per_90)), 2), " avg. per match")

yellow_card <- max(standard$CrdY)
red_card <- max(standard$CrdR)

tackles_won <- max(misc$TklW)
tackles_won_per_game <- paste0(round(as.double(max(misc$TklW) / max(shooting$Mins_Per_90)), 2), " avg. per match")

interceptions <- max(misc$Int)
interceptions_per_match <- paste0(round(as.double(max(misc$Int) / max(shooting$Mins_Per_90)), 2), " avg. per match")

saves <- max(keeper$Saves)
saves_per_match <- paste0(round(as.double(max(keeper$Saves) / max(shooting$Mins_Per_90)), 2), " avg. per match")



# Create image 
final_image <- image_blank(width = 2000, height = 2000, color = canvas_colour)


# Adding Logo To Image
final_image <- image_composite(final_image, team_logo, gravity = "north", offset = "+0+900")
final_image <- image_composite(final_image, second_competition_logo, gravity = "north", offset = "-400+900")
final_image <- image_composite(final_image, competition_logo, gravity = "north", offset = "+400+900")


final_image <- image_annotate(final_image, "BAYER 04 LEVERKUSEN", size = 140, gravity = "north", location = "+000+150", color = "#FE0000", weight = 900)
final_image <- image_annotate(final_image, "2023/24: A DOMESTIC INVINCIBLE SEASON", size = 70, gravity = "north", location = "+000+350", color = "#948453")
final_image <- image_annotate(final_image, season_overview, size = 200, gravity = "north", location = "+000+1400", color = main_text_colour, weight = 900)
final_image <- image_annotate(final_image, "A PRODUCT OF IONIS ANALYTIKA", size = 25, gravity = "north", location = "+000+1950", color = main_text_colour)


print(final_image)

