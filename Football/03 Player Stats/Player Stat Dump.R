### WEBSITES FOR HELP:
### LOGOS: https://1000logos.net/?s
### IMAGE SIZE: https://www.img2go.com/compare-image
### IMAGE RATIO: https://red-route.org/code/image-resizing-calculator


## Package names
packages <- c("dplyr", "magick", "ggplot2", "grid", "hrbrthemes", "stringr", "worldfootballR", "tidyr", "kableExtra", "plotly", "tidyr", "sparkline",
              "formattable", "webshot", "fmsb")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

################################################################

main_font_colour <- "#FFFFFF"
team_font_colour <- "#FFC501"
competition_logo <- image_read("https://mychauffeur.com/_next/static/media/logo2.b3159507.png")
competition_logo <- image_scale(competition_logo, "167.69x200!")

flag_team_logo <- image_read("https://vectorflags.s3.amazonaws.com/flags/es-circle-01.png")
flag_team_logo <- image_scale(flag_team_logo, "800x800!")

background <- "https://img.uefa.com/imgml/uefacom/performancezone/teams-bg/Spain_Desktop.jpg"
player_image <- "https://img.uefa.com/imgml/TP/players/3/2024/cutoff/250082664.png"

team_url <- "https://fbref.com/en/squads/b561dd30/Spain-Men-Stats"

player_name <- "Rodri"


standard <- fb_team_player_stats(team_url, stat_type = 'standard') %>%
  filter(Player == player_name)
shooting <- fb_team_player_stats(team_url, stat_type = 'shooting') %>%
  filter(Player == player_name)
passing <- fb_team_player_stats(team_url, stat_type = 'passing') %>%
  filter(Player == player_name)
passing_type <- fb_team_player_stats(team_url, stat_type = 'passing_types') %>%
  filter(Player == player_name)
possession <- fb_team_player_stats(team_url, stat_type = 'possession') %>%
  filter(Player == player_name)
defense <- fb_team_player_stats(team_url, stat_type = 'defense') %>%
  filter(Player == player_name)
misc <- fb_team_player_stats(team_url, stat_type = 'misc') %>%
  filter(Player == player_name)


### Numbers (Stats)

match_played <- max(standard$MP)
minutes_played <- max(standard$Min_Playing_Time)

goals <- max(shooting$Gls_Standard)
shots <- max(shooting$Sh_Standard)
xg <- max(shooting$xG_Expected)
assists <- max(standard$Ast)
expected_goal_assist <- max(standard$xAG_Expected)

passing_percentage <- max(passing$Cmp_percent_Total)
total_passes <- max(passing$Cmp_Total)
short_passing_percentage <- max(passing$Cmp_percent_Short)
medium_passing_percentage <- max(passing$Cmp_percent_Medium)
long_passing_percentage <- max(passing$Cmp_percent_Long)



shots_on_target <- max(shooting$SoT_Standard)
shots_on_target_percentage <- max(shooting$SoT_percent_Standard)



#### Shooting


#### Canvas
bg <- image_read(path = background)

bg <- image_resize(bg, "2500x2500")


canvas <- image_blank(width = image_info(bg)$width,
                      height = image_info(bg)$height,
                      color = "white")

#### Composite the background image onto the canvas
canvas_with_background <- image_composite(canvas, bg)


#### Player Name
canvas_with_background <- image_annotate(canvas_with_background, player_name, size = 130, weight = 700, gravity = "north", location = "-1060+10", color = main_font_colour)
canvas_with_background <- image_annotate(canvas_with_background, "EURO 2024: Tournament In Numbers", size = 40, weight = 200, gravity = "north", location = "-895+150", color = main_font_colour)

#### Flag or Team Logo
canvas_with_background <- image_composite(canvas_with_background, flag_team_logo, gravity = "north", offset = "-900+300")

#### Player Image
player_image_tag <- image_read(path = player_image)
canvas_with_background <- image_composite(canvas_with_background, player_image_tag, gravity = "north", offset = "-700+200")

#### Competition Logo
canvas_with_background <- image_composite(canvas_with_background, competition_logo, gravity = "north", offset = "+1150+10")

#### Key Stats

## Matches Played
canvas_with_background <- image_annotate(canvas_with_background, match_played, size = 80, gravity = "north", location = "-150+215", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Matches Played", size = 40, gravity = "north", location = "-150+310", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste(minutes_played, "Minutes Played"), size = 20, gravity = "north", location = "-150+360", color = main_font_colour, weight = 200)

## Goals
canvas_with_background <- image_annotate(canvas_with_background, goals, size = 80, gravity = "north", location = "+250+215", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Goals", size = 40, gravity = "north", location = "+250+310", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste(shots, "Shots Taken"), size = 20, gravity = "north", location = "+250+360", color = main_font_colour, weight = 200)

## Assists
canvas_with_background <- image_annotate(canvas_with_background, assists, size = 80, gravity = "north", location = "+650+215", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Assists", size = 40, gravity = "north", location = "+650+310", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste(expected_goal_assist, "Expected Goal Assists"), size = 20, gravity = "north", location = "+650+360", color = main_font_colour, weight = 200)


# Take On Success
canvas_with_background <- image_annotate(canvas_with_background, paste0(max(possession$Succ_percent_Take_Ons), "%"), size = 80, gravity = "north", location = "+250+450", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Take On Success (%)", size = 40, gravity = "north", location = "+250+545", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste0(max(possession$Succ_Take_Ons), "/", max(possession$Att_Take_Ons), " Take Ons Successful"), size = 20, gravity = "north", location = "+250+595", color = main_font_colour, weight = 200)

# Interceptions
canvas_with_background <- image_annotate(canvas_with_background, max(defense$Int), size = 80, gravity = "north", location = "+250+685", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Interceptions", size = 40, gravity = "north", location = "+250+780", color = main_font_colour, weight = 200)

# Clearances
canvas_with_background <- image_annotate(canvas_with_background, max(defense$Clr), size = 80, gravity = "north", location = "+250+920", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Clearances", size = 40, gravity = "north", location = "+250+1015", color = main_font_colour, weight = 200)


# Final Third Passes
canvas_with_background <- image_annotate(canvas_with_background, max(passing$Final_Third), size = 80, gravity = "north", location = "+650+450", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Final 3rd Passes", size = 40, gravity = "north", location = "+650+545", color = main_font_colour, weight = 200)

# Progressive Passes
canvas_with_background <- image_annotate(canvas_with_background, max(passing$PrgP), size = 80, gravity = "north", location = "+650+685", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Progressive Passes", size = 40, gravity = "north", location = "+650+780", color = main_font_colour, weight = 200)


# Fouls
canvas_with_background <- image_annotate(canvas_with_background, max(misc$Fls), size = 80, gravity = "north", location = "+650+920", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Fouls", size = 40, gravity = "north", location = "+650+1015", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste0(max(misc$CrdY), " Yellow Cards + ", max(misc$CrdR), " Red Cards"), size = 20, gravity = "north", location = "+650+1065", color = main_font_colour, weight = 200)


## Passing
canvas_with_background <- image_annotate(canvas_with_background, paste0(passing_percentage, "%"), size = 80, gravity = "north", location = "+1050+215", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Passing (%)", size = 40, gravity = "north", location = "+1050+310", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste(total_passes, "Passes Completed"), size = 20, gravity = "north", location = "+1050+360", color = main_font_colour, weight = 200)

canvas_with_background <- image_annotate(canvas_with_background, paste0(short_passing_percentage, "%"), size = 80, gravity = "north", location = "+1050+450", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Short Passing (%)", size = 40, gravity = "north", location = "+1050+545", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste0(max(passing$Cmp_Short), "/", max(passing$Att_Short), " Passes Completed"), size = 20, gravity = "north", location = "+1050+595", color = main_font_colour, weight = 200)

canvas_with_background <- image_annotate(canvas_with_background, paste0(medium_passing_percentage, "%"), size = 80, gravity = "north", location = "+1050+685", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Medium Passing (%)", size = 40, gravity = "north", location = "+1050+780", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste0(max(passing$Cmp_Medium), "/", max(passing$Att_Medium), " Passes Completed"), size = 20, gravity = "north", location = "+1050+830", color = main_font_colour, weight = 200)

canvas_with_background <- image_annotate(canvas_with_background, paste0(long_passing_percentage, "%"), size = 80, gravity = "north", location = "+1050+920", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Long Passing (%)", size = 40, gravity = "north", location = "+1050+1015", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste0(max(passing$Cmp_Long), "/", max(passing$Att_Long), " Passes Completed"), size = 20, gravity = "north", location = "+1050+1065", color = main_font_colour, weight = 200)


## Shooting
canvas_with_background <- image_annotate(canvas_with_background, paste0(shots_on_target_percentage, "%"), size = 80, gravity = "north", location = "-150+450", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Shots On Target (%)", size = 40, gravity = "north", location = "-150+545", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste(shots_on_target, "Shots On Target"), size = 20, gravity = "north", location = "-150+595", color = main_font_colour, weight = 200)

## Tackles
canvas_with_background <- image_annotate(canvas_with_background, paste0(max(defense$Tkl_percent_Challenges), "%"), size = 80, gravity = "north", location = "-150+685", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Tackle Success (%)", size = 40, gravity = "north", location = "-150+780", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste0(max(defense$TklW_Tackles), "/", max(defense$Tkl_Tackles), " Tackles Won"), size = 20, gravity = "north", location = "-150+830", color = main_font_colour, weight = 200)

## Blocks
canvas_with_background <- image_annotate(canvas_with_background, max(defense$Blocks_Blocks), size = 80, gravity = "north", location = "-150+920", color = team_font_colour, weight = 800)
canvas_with_background <- image_annotate(canvas_with_background, "Blocks", size = 40, gravity = "north", location = "-150+1015", color = main_font_colour, weight = 200)
canvas_with_background <- image_annotate(canvas_with_background, paste(max(defense$Pass_Blocks), " Pass Blocks + ", max(defense$Sh_Blocks), " Shot Blocks"), size = 20, gravity = "north", location = "-150+1065", color = main_font_colour, weight = 200)

# Display the canvas with the background
print(canvas_with_background)


# Save Image
image_write(canvas_with_background, path = paste(player_name, ".png"), format = "png", density = 300)

