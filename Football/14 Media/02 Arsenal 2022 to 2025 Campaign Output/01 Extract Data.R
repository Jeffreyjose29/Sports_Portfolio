## Package names
packages <- c("dplyr", "readxl", "purrr", "sjmisc", "worldfootballR", "plyr", "magick", "httr", "jsonlite", "kableExtra", "webshot", "ggplot2",
              "stringr", "StatsBombR", "ggsoccer", "zoo")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))


########################################################################################################################

primary_colour <- "#DB0007"
secondary_colour <- "#FFFFFF"

image_file_location <- "C:/Users/jeffr/Downloads/New folder/"
club_logo <- image_read(
  paste0(image_file_location, "Arsenal.png")) %>%
  image_resize("275x275")

########################################################################################################################

# Arsenal 2023/2024 season
season_2019_2020 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2024, tier = "1st")

season_2019_2020 <- season_2019_2020 %>%
  filter(Home == "Arsenal" | Away == "Arsenal")

season_2019_2020_formatted <- rbind(
  season_2019_2020 %>%
  filter(Home == "Arsenal") %>%
  dplyr::rename("Match Day" = Wk, "Team" = Home, "GF" = HomeGoals, "xG" = Home_xG, "GA" = AwayGoals, "xGA" = Away_xG) %>%
  select(`Match Day`, `Team`, `GF`, `xG`, `GA`, `xGA`)
  ,
  season_2019_2020 %>%
    filter(Away == "Arsenal") %>%
    dplyr::rename("Match Day" = Wk, "Team" = Away, "GF" = AwayGoals, "xG" = Away_xG, "GA" = HomeGoals, "xGA" = Home_xG) %>%
    select(`Match Day`, `Team`, `GF`, `xG`, `GA`, `xGA`)
) %>%
  arrange(as.numeric(`Match Day`))


# 5-Game Rolling xG vs. xGA
xg_vs_xGA <- season_2019_2020_formatted %>%
  mutate(`Match Day` = as.numeric(`Match Day`)) %>%
  arrange(`Match Day`) %>%
  mutate(
    xG = rollmean(xG, k = 3, fill = NA, align = "right"),
    xGA = rollmean(xGA, k = 3, fill = NA, align = "right")  # Assuming xGA is xA
  ) %>%
  select(`Match Day`, xG, xGA) %>%
  pivot_longer(cols = -`Match Day`, 
               names_to = "Metric", 
               values_to = "Value") %>%
  ggplot(aes(x = `Match Day`, y = Value, color = Metric)) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("xG" = primary_colour, "xGA" = secondary_colour)) +
  labs(x = "Match Day",
       title = "Expected Goals vs. Expected Goals Against") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1b2326", color = NA),
    plot.background = element_rect(fill = "#1b2326", color = NA),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed major vertical gridlines
    panel.grid.minor.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor vertical gridlines
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed horizontal gridlines
    panel.grid.minor.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor horizontal gridlines (optional)
    axis.title = element_text(size = 16, color = "grey"),
    axis.text = element_text(size = 14, color = "grey"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color = "grey"),
    plot.title = element_text(hjust = 0.5, color = "#FFFFFF", size = 16)
  )

gg_grob_runrate <- ggplotGrob(xg_vs_xGA)
gg_image_rolling_xg_xga_prev <- image_graph(width = 1000, height = 600, res = 98)
grid::grid.draw(gg_grob_runrate)
dev.off()
    
    
    
# Goals vs. Against
g_vs_ga <- season_2019_2020_formatted %>%
  mutate(`Match Day` = as.numeric(`Match Day`)) %>%
  arrange(`Match Day`) %>%
  mutate(
    GF = rollmean(GF, k = 3, fill = NA, align = "right"),
    GA = rollmean(GA, k = 3, fill = NA, align = "right")  # Assuming xGA is xA
  ) %>%
  select(`Match Day`, GF, GA) %>%
  pivot_longer(cols = -`Match Day`, 
               names_to = "Metric", 
               values_to = "Value") %>%
  ggplot(aes(x = `Match Day`, y = Value, color = Metric)) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("GF" = primary_colour, "GA" = secondary_colour)) +
  labs(x = "Match Day",
       title = "Goals Scored vs. Goals Conceded") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1b2326", color = NA),
    plot.background = element_rect(fill = "#1b2326", color = NA),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed major vertical gridlines
    panel.grid.minor.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor vertical gridlines
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed horizontal gridlines
    panel.grid.minor.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor horizontal gridlines (optional)
    axis.title = element_text(size = 16, color = "grey"),
    axis.text = element_text(size = 14, color = "grey"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color = "grey"),
    plot.title = element_text(hjust = 0.5, color = "#FFFFFF", size = 16)
  )

gg_grob_runrate <- ggplotGrob(g_vs_ga)
gg_image_rolling_g_vs_ga_prev <- image_graph(width = 1000, height = 600, res = 98)
grid::grid.draw(gg_grob_runrate)
dev.off()

# Goals vs. xG
g_vs_xg <- season_2019_2020_formatted %>%
  mutate(`Match Day` = as.numeric(`Match Day`)) %>%
  arrange(`Match Day`) %>%
  mutate(
    GF = rollmean(GF, k = 3, fill = NA, align = "right"),
    xG = rollmean(xG, k = 3, fill = NA, align = "right")  # Assuming xGA is xA
  ) %>%
  select(`Match Day`, GF, xG) %>%
  pivot_longer(cols = -`Match Day`, 
               names_to = "Metric", 
               values_to = "Value") %>%
  ggplot(aes(x = `Match Day`, y = Value, color = Metric)) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("GF" = primary_colour, "xG" = secondary_colour)) +
  labs(x = "Match Day",
       title = "Goals Scored vs. Expected Goals") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1b2326", color = NA),
    plot.background = element_rect(fill = "#1b2326", color = NA),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed major vertical gridlines
    panel.grid.minor.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor vertical gridlines
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed horizontal gridlines
    panel.grid.minor.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor horizontal gridlines (optional)
    axis.title = element_text(size = 16, color = "grey"),
    axis.text = element_text(size = 14, color = "grey"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color = "grey"),
    plot.title = element_text(hjust = 0.5, color = "#FFFFFF", size = 16)
  )

gg_grob_runrate <- ggplotGrob(g_vs_xg)
gg_image_rolling_g_vs_xg_prev <- image_graph(width = 1000, height = 600, res = 98)
grid::grid.draw(gg_grob_runrate)
dev.off()

season_output_text_prev_season <- paste0("Goals: ", sum(season_2019_2020_formatted$GF), " | xG: ", sum(season_2019_2020_formatted$xG), 
                             " | Goals Against: ", sum(season_2019_2020_formatted$GA), " | xGA: ", sum(season_2019_2020_formatted$xGA))




############ Current Season

# Arsenal 2024/2025 season
season_2024_2025 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2025, tier = "1st")

season_2024_2025 <- season_2024_2025 %>%
  filter(Home == "Arsenal" | Away == "Arsenal")

season_2024_2025_formatted <- rbind(
  season_2024_2025 %>%
    filter(Home == "Arsenal") %>%
    dplyr::rename("Match Day" = Wk, "Team" = Home, "GF" = HomeGoals, "xG" = Home_xG, "GA" = AwayGoals, "xGA" = Away_xG) %>%
    select(`Match Day`, `Team`, `GF`, `xG`, `GA`, `xGA`)
  ,
  season_2024_2025 %>%
    filter(Away == "Arsenal") %>%
    dplyr::rename("Match Day" = Wk, "Team" = Away, "GF" = AwayGoals, "xG" = Away_xG, "GA" = HomeGoals, "xGA" = Home_xG) %>%
    select(`Match Day`, `Team`, `GF`, `xG`, `GA`, `xGA`)
) %>%
  arrange(as.numeric(`Match Day`)) %>%
  drop_na(GF)


# 5-Game Rolling xG vs. xGA
xg_vs_xGA <- season_2024_2025_formatted %>%
  mutate(`Match Day` = as.numeric(`Match Day`)) %>%
  arrange(`Match Day`) %>%
  mutate(
    xG = rollmean(xG, k = 3, fill = NA, align = "right"),
    xGA = rollmean(xGA, k = 3, fill = NA, align = "right")  # Assuming xGA is xA
  ) %>%
  select(`Match Day`, xG, xGA) %>%
  pivot_longer(cols = -`Match Day`, 
               names_to = "Metric", 
               values_to = "Value") %>%
  ggplot(aes(x = `Match Day`, y = Value, color = Metric)) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("xG" = primary_colour, "xGA" = secondary_colour)) +
  labs(x = "Match Day",
       title = "Expected Goals vs. Expected Goals Against") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1b2326", color = NA),
    plot.background = element_rect(fill = "#1b2326", color = NA),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed major vertical gridlines
    panel.grid.minor.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor vertical gridlines
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed horizontal gridlines
    panel.grid.minor.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor horizontal gridlines (optional)
    axis.title = element_text(size = 16, color = "grey"),
    axis.text = element_text(size = 14, color = "grey"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color = "grey"),
    plot.title = element_text(hjust = 0.5, color = "#FFFFFF", size = 16)
  )

gg_grob_runrate <- ggplotGrob(xg_vs_xGA)
gg_image_rolling_xg_xga_curr <- image_graph(width = 1000, height = 600, res = 98)
grid::grid.draw(gg_grob_runrate)
dev.off()

# Goals vs. Against
gf_vs_ga <- season_2024_2025_formatted %>%
  mutate(`Match Day` = as.numeric(`Match Day`)) %>%
  arrange(`Match Day`) %>%
  mutate(
    GF = rollmean(GF, k = 3, fill = NA, align = "right"),
    GA = rollmean(GA, k = 3, fill = NA, align = "right")  # Assuming xGA is xA
  ) %>%
  select(`Match Day`, GF, GA) %>%
  pivot_longer(cols = -`Match Day`, 
               names_to = "Metric", 
               values_to = "Value") %>%
  ggplot(aes(x = `Match Day`, y = Value, color = Metric)) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("GF" = primary_colour, "GA" = secondary_colour)) +
  labs(x = "Match Day",
       title = "Goals Scored vs. Goals Conceded") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1b2326", color = NA),
    plot.background = element_rect(fill = "#1b2326", color = NA),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed major vertical gridlines
    panel.grid.minor.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor vertical gridlines
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed horizontal gridlines
    panel.grid.minor.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor horizontal gridlines (optional)
    axis.title = element_text(size = 16, color = "grey"),
    axis.text = element_text(size = 14, color = "grey"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color = "grey"),
    plot.title = element_text(hjust = 0.5, color = "#FFFFFF", size = 16)
  )


gg_grob_runrate <- ggplotGrob(gf_vs_ga)
gg_image_rolling_gf_vs_ga_curr <- image_graph(width = 1000, height = 600, res = 98)
grid::grid.draw(gg_grob_runrate)
dev.off()


# Goals vs. xG
g_vs_xg <- season_2024_2025_formatted %>%
  mutate(`Match Day` = as.numeric(`Match Day`)) %>%
  arrange(`Match Day`) %>%
  mutate(
    GF = rollmean(GF, k = 3, fill = NA, align = "right"),
    xG = rollmean(xG, k = 3, fill = NA, align = "right")  # Assuming xGA is xA
  ) %>%
  select(`Match Day`, GF, xG) %>%
  pivot_longer(cols = -`Match Day`, 
               names_to = "Metric", 
               values_to = "Value") %>%
  ggplot(aes(x = `Match Day`, y = Value, color = Metric)) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("GF" = primary_colour, "xG" = secondary_colour)) +
  labs(x = "Match Day",
       title = "Goals Scored vs. Expected Goals") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1b2326", color = NA),
    plot.background = element_rect(fill = "#1b2326", color = NA),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed major vertical gridlines
    panel.grid.minor.x = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor vertical gridlines
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed horizontal gridlines
    panel.grid.minor.y = element_line(linetype = "dashed", color = "grey50"),  # Dashed minor horizontal gridlines (optional)
    axis.title = element_text(size = 16, color = "grey"),
    axis.text = element_text(size = 14, color = "grey"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color = "grey"),
    plot.title = element_text(hjust = 0.5, color = "#FFFFFF", size = 16)
  )


gg_grob_runrate <- ggplotGrob(g_vs_xg)
gg_image_rolling_g_vs_xg_curr <- image_graph(width = 1000, height = 600, res = 98)
grid::grid.draw(gg_grob_runrate)
dev.off()

season_output_text_2025 <- paste0("Goals: ", sum(season_2024_2025_formatted$GF), " | xG: ", sum(season_2024_2025_formatted$xG), 
                             " | Goals Against: ", sum(season_2024_2025_formatted$GA), " | xGA: ", sum(season_2024_2025_formatted$xGA))
