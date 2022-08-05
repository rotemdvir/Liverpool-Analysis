# Liverpool 
# August 2022

library(tidyverse)
library(readxl)
library(png)
library(ggdark)

# load data
dat <- read_excel("Documents/R_Proj/Data/Liverpool/DataFiles/Def_Poss_Data.xlsx")

# add ranking values
dat1 <- dat %>% 
  group_by(Season) %>% 
  mutate(PressAttRate = round(PressAtt/TotalPress,2),
         PressRank = rank(-PressPerc),
         AttRatRank = rank(-PressAttRate),
         PressAttRank = rank(-PressAtt),
         PossRank = rank(-Poss),
         PossGap = round((Poss-max(Poss))/Poss*100,1)) %>% 
  arrange(Season,PossRank)

# keep Liverpool data
dat2 <- dat1 %>%
  filter(Squad == "Liverpool") 

# Add team logo
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), height = unit(1, "npc"), interpolate = TRUE)}

pic <- get_png("Documents/R_Proj/Data/Liverpool/liv.png")

# Plot all
p <- ggplot(dat2, aes(Season, PressAtt)) +
  geom_bar(stat = "identity", color = "#69b3a2", alpha = 0.4, width = 0.7) + xlab("") + ylab("Pressures (Attack 3rd)") +
  geom_text(aes(label = paste("Pressure \n Rank:", PressAttRank)), vjust = -0.2, size = 3.5) + 
  geom_line(aes(y = Poss*20, group = 1), color = "red", size = 2) +
  geom_point(aes(y=Poss*20)) +
  geom_text(aes(y=Poss*20, label = paste("Poss. \n Rank:", PossRank), vjust = 1.3), size = 3.5) +
  scale_y_continuous(sec.axis = sec_axis(~. /20, name = "Possession")) +
  labs(title = "Gegenpressing (and?) Possession:", 
       subtitle = "How Liverpool increased focus on ball possession (Pep style?) while staying number 1 in pressuring the opponent defense (Klopp style!)") +
  dark_theme_classic() 
  

p + annotation_custom(pic, xmin = 5.3, xmax = 5.5, ymin = 1800, ymax = 2000) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        plot.title = element_text(vjust = 4, size = 18, face = "bold"),
        plot.subtitle = element_text(vjust = 3, size = 14, face = "italic")) 

# Save with high-end resolution
ggsave("R_Proj/Data/Liverpool/Liverpool2.jpeg", width = 20, height = 10, units = "in", dpi = 300)

