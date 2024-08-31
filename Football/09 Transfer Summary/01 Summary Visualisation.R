## Package names
packages <- c("dplyr", "readxl", "purrr", "sjmisc", "worldfootballR", "plyr", "magick", "httr", "jsonlite", "kableExtra", "webshot", "ggplot2",
              "stringr", "StatsBombR", "ggsoccer", "ggimage", "scales")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))


bg_colour <- "#0a1820"
text_colour <- "#FFFFFF"
heading_text <- "ENGLISH PREMIER LEAGUE TRANSFER ACTIVITY"
sub_heading <- "Summer Transfer Window 24/25 | Data From Fotmob.com | Viz Created By Jeffrey Jose"


# Plot the chart for each time spending and profit
transfer_gg <- transfer_combined %>%
  ggplot(aes(x = Club, y = value)) +
  geom_bar(stat = "identity", aes(fill = value > 0), width = 0.5) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_fill_manual(values = c("TRUE" = "#06a62a", "FALSE" = "#d61212")) +
  labs(x = "Club Logo", y = "Value", fill = "Value > 0") +
  guides(fill = "none") +
  scale_x_discrete(name = NULL, labels = labels) +
  theme(
    axis.text.x = ggtext::element_markdown(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = text_colour),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    panel.background = element_rect(fill = bg_colour, colour = bg_colour),
    panel.border = element_blank(),
    panel.grid.major = element_blank(), # Removes major gridlines
    panel.grid.minor = element_blank(), # Removes minor gridlines
  )

gg_grob_xg <- ggplotGrob(transfer_gg)
gg_image_xg <- image_graph(width = 1000, height = 400, res = 98)
grid::grid.draw(gg_grob_xg)
dev.off()

## Creating Canvas
final_image <- image_blank(width = 2000, height = 1000, color = bg_colour)


# Adding Heading Text
final_image <- image_annotate(final_image, heading_text, gravity = "north", location = "+0+30", weight = 900, size = 60, color = text_colour)
final_image <- image_annotate(final_image, sub_heading, gravity = "north", location = "+0+120", weight = 300, size = 25, color = text_colour)

final_image <- image_annotate(final_image, paste0("€", league_outgoing_total), gravity = "north", location = "-310+240", weight = 900, size = 90, color = text_colour)
final_image <- image_annotate(final_image, "Total Received", gravity = "north", location = "-310+350", weight = 300, size = 25, color = text_colour)

final_image <- image_annotate(final_image, paste0("€", league_incoming_total), gravity = "north", location = "+310+240", weight = 900, size = 90, color = text_colour)
final_image <- image_annotate(final_image, "Total Spend", gravity = "north", location = "+310+350", weight = 300, size = 25, color = text_colour)


final_image <- image_annotate(final_image, paste0("€", league_net_value), gravity = "north", location = "+0+410", weight = 900, size = 90, color = text_colour)
final_image <- image_annotate(final_image, "Net Spend", gravity = "north", location = "+0+520", weight = 300, size = 25, color = text_colour)


final_image <- image_composite(final_image, gg_image_xg, gravity = "north", offset = "-0+570")


# Player Image 1

# https://imageresizer.com/resize/edito

player1 <- image_read("https://i.ibb.co/41hK8KS/Juli-n-lvarez-Footy-Renders.png")
player1 <- image_scale(player1, "462x600")


final_image <- image_annotate(final_image, "Julián Álvarez", gravity = "north", location = "-750+300", weight = 100, size = 15, color = text_colour)
final_image <- image_annotate(final_image, "Transfer Fee: €70.00M", gravity = "north", location = "-750+340", weight = 300, size = 15, color = text_colour)
final_image <- image_annotate(final_image, "Manchester City F.C > Atlético de Madrid", gravity = "north", location = "-750+320", weight = 300, size = 15, color = text_colour)

final_image <- image_composite(final_image, player1, gravity = "north", offset = "-800+395")


# Player Image 2

# https://imageresizer.com/resize/edito

player2 <- image_read("https://i.ibb.co/MP8YQVm/Dominic-Solanke-Footy-Renders.png")
player2 <- image_scale(player2, "275x600")


final_image <- image_annotate(final_image, "Dominic Solanke", gravity = "north", location = "+750+300", weight = 100, size = 15, color = text_colour)
final_image <- image_annotate(final_image, "Transfer Fee: €42.00M", gravity = "north", location = "+750+340", weight = 300, size = 15, color = text_colour)
final_image <- image_annotate(final_image, "A.F.C. Bournemouth > Tottenham Hotspur F.C.", gravity = "north", location = "+750+320", weight = 300, size = 15, color = text_colour)


final_image <- image_composite(final_image, player2, gravity = "north", offset = "+800+395")


print(final_image)
