## Package names
packages <- c("worldfootballR", "tidyverse", "forcats", "glue", "magick", "httr", "jsonlite", "kableExtra", "webshot")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Help Link: https://www.gettingbluefingers.com/tutorials/RadarPizzaChart

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

fbref_link <- "https://fbref.com/en/players/8c90fd7a/Victor-Osimhen"
team_colour <- "#02a1db"
player_name <- "Victor Osimhen"
team <- "SSC Napoli"
team_logo <- "https://i.ibb.co/1rbyPmK/kisspng-s-s-c-napoli-assembly-of-christ-school-logo-footb-napoli-fc-logo-bing-images-5be1f848d2e3b8.png"
player_image <- "https://i.ibb.co/jZL6CVP/Victor-Osimhen-Footy-Renders-2.png"
canvas_colour <- "#F2F4F5"
text_colour <- "#000000"

setwd("C:/Users/jeffr/OneDrive/Desktop/Github Activities/Sports_Analysis/Football/10 Player Radar V2/Figure")


df <- fb_player_scouting_report(fbref_link, pos_versus = "primary")

df <- df %>%
  filter(scouting_period == "Last 365 Days Men's Big 5 Leagues, UCL, UEL")

player_name <- unique(df$Player)


# Attack: "Goals", "Assists", "xG: Expected Goals", "Shots Total", "Shots on Target %", "Goals/Shot",
#"Pass Completion %", "Touches (Att Pen)", "Successful Take-On %", "% of Aerials Won"

df_selected <- df %>%
  filter(Statistic %in% c("Goals", "Assists", "xG: Expected Goals", "npxG: Non-Penalty xG", "Shots Total", "Shots on Target %", "Goals/Shot",
                        "Pass Completion %", "Touches (Att Pen)", "Successful Take-On %", "% of Aerials Won",
                        "Progressive Passes Rec"))


df_selected <- df_selected %>% 
  mutate(stat=case_when(Statistic == "Goals"|
                          Statistic == "Assists"|
                          Statistic == "xG: Expected Goals"|
                          Statistic == "npxG: Non-Penalty xG"|
                          Statistic == "Shots Total"|
                          Statistic == "Shots on Target %"|
                          Statistic == "Goals/Shot" ~ "Attacking",
                          Statistic == "Pass Completion %" |
                          Statistic == "Touches (Att Pen)" |
                          Statistic == "Successful Take-On %"|
                          Statistic == "% of Aerials Won" |
                          Statistic == "Progressive Passes Rec" ~ "Possession",
                        TRUE ~ "Attacking"))


df_selected$Statistic <- gsub(" ","\n",df_selected$Statistic)

# Plot
#tiff("Viktor Gyokeres.tiff", units="in", width=20, height=10, res=300)
plot_gg <- ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                      
  geom_bar(aes(y=100),fill=canvas_colour,stat="identity",width=1,colour=text_colour,                
           alpha=1,linetype="dashed") +                                                                          
  geom_bar(stat="identity",width=1,fill=team_colour,colour="#F2F4F5") +   
  geom_hline(yintercept=25, colour="#F2F4F5",linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=50, colour="#F2F4F5",linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=75, colour="#F2F4F5",linetype="longdash",alpha=0.5)+ 
  geom_hline(yintercept=100, colour="#F2F4F5",alpha=0.5)+ 
  coord_polar() +                                                                     
  geom_label(aes(label=Per90),fill=team_colour,size=5,color="white",show.legend = FALSE)+     
  scale_fill_manual(values=c("Possession" = "#D70232",                                  
                             "Attacking" = "#1A78CF",
                             "Defending" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-10,100))+                                              
  #labs(fill="",   
       #caption = "Data from StatsBomb via FBref",     
       #remove legend title
       #title=glue("{df_selected$Player[1]} | Sporting CP"),
       #subtitle = glue::glue("Compared To Attackers In Last 365 Days Men's Big 5 Leagues, UCL, UEL | Stats Per 90")) +                                               
  
  theme_minimal() +                                                                     
  theme(plot.background = element_rect(fill = canvas_colour,color = canvas_colour),
        panel.background = element_rect(fill = canvas_colour,color = canvas_colour),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, colour = text_colour),
        plot.subtitle = element_text(hjust=0.5,size=10),
        plot.caption = element_text(hjust=0.5,size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,2,2,2)) 
#dev.off()

gg_grob_xg <- ggplotGrob(plot_gg)
gg_image_xg <- image_graph(width = 1000, height = 1000, res = 98)
grid::grid.draw(gg_grob_xg)
dev.off()


final_image <- image_blank(width = 2000, height = 1000, color = canvas_colour)
final_image <- image_composite(final_image, gg_image_xg, gravity = "north", offset = "-500+0")


final_image <- image_annotate(final_image, player_name, gravity = "north", location = "+500+140", weight = 500, size = 100, color = text_colour)
final_image <- image_annotate(final_image, team, gravity = "north", location = "+500+250", weight = 200, size = 60, color = team_colour)
final_image <- image_annotate(final_image, "Compared To Attackers In Last 365 Days Men's Big 5 Leagues, UCL, UEL | Stats Per 90", 
                              gravity = "north", location = "+500+330", weight = 200, size = 20, color = text_colour)


# Club Logo
team_logo_out <- image_scale(image_read(team_logo), "65x80")
final_image <- image_composite(final_image, team_logo_out, gravity = "north", offset = "+500+65")

# Player Image
player_image_out <- image_scale(image_read(player_image), "507x700")
final_image <- image_composite(final_image, player_image_out, gravity = "north", offset = "+500+375")


final_image
