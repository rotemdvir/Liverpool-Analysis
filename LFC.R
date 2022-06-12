# Liverpool 
# June 2022

library(tidyverse)
library(readxl)
library(hrbrthemes)
library(kableExtra)
library(png)
library(ggridges)
library(ggalt)
library(nbapalettes)

# Minutes load comps
t_dat <- read_excel("Documents/R_Proj/Data/Liverpool/2122_AllPlayers.xlsx", sheet = "minutes")

kbl(t_dat, caption = "Minutes Comparison: Top 3", booktabs = T, align = "lccc") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(position = "center") %>%
  pack_rows("Liverpool: 2021-2022", 1, 3) %>%
  pack_rows("Man. United: 1998-1999", 4, 6) %>%
  column_spec(1,bold = T) %>% 
  column_spec(4,color = "white", bold = T,
              background = ifelse(t_dat$Minutes > 4000, "red","green"))

## Part 2: 2021-22 data
# Comp to Europe top
g_dat <- read_excel("Documents/R_Proj/Data/Liverpool/2122_AllPlayers.xlsx", sheet = "goal_comp")

g_dat <- g_dat %>%
  mutate(gf_diff = round(GF-xG,1),
         ga_diff = round(GA-xGA,1))

# fig 1: xG; xGA with logos
g_dat2 <- g_dat %>% 
  dplyr::select(Team,League,xG,GF) %>%
  gather(stat,total,xG:GF,-Team,-League) %>%
  arrange(Team)

pp <- ggplot(g_dat2, aes(x = reorder(Team, -total), y = total)) +
  geom_bar(aes(fill = stat), stat = "identity", width = 0.8, position = position_dodge(0.8)) +
  geom_text(data = filter(g_dat2, stat == "xG"),
            aes(label = total),size=3.5,hjust=-0.1,vjust=1.3,color = "black",fontface="bold") +
  geom_text(data = filter(g_dat2, stat == "GF"),
            aes(label = total),size=3.5,hjust=1.5,vjust=1.3,color = "black",fontface="bold") +
  xlab("") + ylab("") + labs(fill = "") +
  scale_fill_manual(values = c("darkgrey","skyblue")) +
  scale_x_discrete(labels = c("Bayern Munich" = "B.Munich", "Real Madrid" = "R.Madrid")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.background = element_rect(size = 0.55, linetype = "solid", colour = "black"))

# Adding logos
# Function to pull png logos
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), height = unit(2.5, "npc"), interpolate = TRUE)}

# Logos located locally
l <- get_png("Documents/R_Proj/Data/Liverpool/liv.png")
l1 <- get_png("Documents/R_Proj/Data/Liverpool/city.png")
l2 <- get_png("Documents/R_Proj/Data/Liverpool/psg.png")
l3 <- get_png("Documents/R_Proj/Data/Liverpool/inter.png")
l4 <- get_png("Documents/R_Proj/Data/Liverpool/real.png")
l5 <- get_png("Documents/R_Proj/Data/Liverpool/bvb.png")
l6 <- get_png("Documents/R_Proj/Data/Liverpool/milan.png")
l7 <- get_png("Documents/R_Proj/Data/Liverpool/barca.png")
l8 <- get_png("Documents/R_Proj/Data/Liverpool/marseille.png")
l9 <- get_png("Documents/R_Proj/Data/Liverpool/bayern.png")

# Add logos to figure (define location of plot)
pp +
  annotation_custom(l, xmin = 2.5, xmax = 3.5, ymin = 97, ymax = 101) +
  annotation_custom(l1, xmin = 0.5, xmax = 1.5, ymin = 102, ymax = 107) +
  annotation_custom(l2, xmin = 3.5, xmax = 4.5, ymin = 93, ymax = 98) +
  annotation_custom(l3, xmin = 4.5, xmax = 5.5, ymin = 87, ymax = 91) +
  annotation_custom(l4, xmin = 5.5, xmax = 6.5, ymin = 84, ymax = 89) +
  annotation_custom(l5, xmin = 6.5, xmax = 7.5, ymin = 88, ymax = 92) +
  annotation_custom(l6, xmin = 7.5, xmax = 8.5, ymin = 73, ymax = 78) +
  annotation_custom(l7, xmin = 8.5, xmax = 9.5, ymin = 71, ymax = 76) +
  annotation_custom(l8, xmin = 9.5, xmax = 10.5, ymin = 67, ymax = 71) +
  annotation_custom(l9, xmin = 1.5, xmax = 2.5, ymin = 100, ymax = 105) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1,1,1,1), "lines")) 

# Fig 2: dumbbell of diffs in offense (xG and GF)
ggplot(g_dat) +
  labs(x=NULL, y=NULL,title = "Comparing Europe's finest clubs: Offensive Performance",
       subtitle = "Expected and goals scored") +
  geom_dumbbell(aes(y = reorder(Team, GF), x = xG, xend = GF), size = 1.5, color="#b2b2b2", 
                size_x=3, size_xend = 3, colour_x = "red", colour_xend = "blue") +
  geom_text(data = filter(g_dat, Team == "Man. City"), 
            aes(x=GF, y=Team, label = "Goals"), color = "blue", size = 3, vjust = -1.5, fontface = "bold") +
  geom_text(data = filter(g_dat, Team == "Man. City"),
            aes(x=xG, y=Team, label = "X.Goals"), color = "red", size = 3, vjust = -1.5, fontface = "bold") +
  geom_text(aes(x=xG, y=Team, label=xG), color="red", size=2.75, vjust=2.5) +
  geom_text(color="blue", size=2.75, vjust=2.5,aes(x=GF, y=Team, label=GF)) +
  geom_rect(aes(xmin=102, xmax=106, ymin=-Inf, ymax=Inf), fill="darkcyan") +
  geom_text(aes(label=gf_diff, y=Team, x=104), color = "white", fontface="bold", size=3.3) +
  geom_text(data=filter(g_dat, Team=="Man. City"),
            aes(x=104, y=Team, label="Diff."),
            color="white", size=3.5, vjust=-2, fontface="bold") +
  scale_x_continuous(expand=c(0,0), limits=c(55,108)) +
  scale_y_discrete(expand=c(0.1,0)) +
  theme_classic() + theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)))

# Fig 3: dumbbell of diffs in defense (xGA and GA)
ggplot(g_dat) +
  labs(x=NULL, y=NULL,title = "Comparing Europe's finest clubs: Defensive Performance",
       subtitle = "Goals against: Expected and actual") +
  geom_dumbbell(aes(y = reorder(Team, GA), x = xGA, xend = GA), size = 1.5, color="#b2b2b2", 
                size_x=3, size_xend = 3, colour_x = "red", colour_xend = "blue") +
  geom_text(data = filter(g_dat, Team == "Dortmund"), 
            aes(x=GA, y=Team, label = "Goals Against"), color = "blue", size = 3, vjust = -1.5, fontface = "bold") +
  geom_text(data = filter(g_dat, Team == "Dortmund"),
            aes(x=xGA, y=Team, label = "X.Goals Against"), color = "red", size = 3, vjust = -1.5, fontface = "bold") +
  geom_text(aes(x=xGA, y=Team, label=xGA), color="red", size=2.75, vjust=2.5) +
  geom_text(color="blue", size=2.75, vjust=2.5,aes(x=GA, y=Team, label=GA)) +
  geom_rect(aes(xmin=56, xmax=60, ymin=-Inf, ymax=Inf), fill="darkcyan") +
  geom_text(aes(label=ga_diff, y=Team, x=58), color = "white", fontface="bold", size=3.3) +
  geom_text(data=filter(g_dat, Team=="Dortmund"),
            aes(x=58, y=Team, label="Diff."),
            color="white", size=3.5, vjust=-2, fontface="bold") +
  scale_x_continuous(expand=c(0,0), limits=c(25,61)) +
  scale_y_discrete(expand=c(0.1,0)) +
  theme_classic() + theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)))

# EPL comp: finance and performance
epl_dat <- read_excel("Documents/R_Proj/Data/Liverpool/2122_AllPlayers.xlsx", sheet = "EPL_comp")
p <- c("£")

epl_dat %>%
  filter(season == "2021-22" | season == "2020-21" | season == "2019-20" | season == "2018-19") %>%
  dplyr::select(-season) %>%
  kbl(col.names = c("",p, "Pts.","GoalDiff"), booktabs = T, caption = "EPL Powerhouses: Comparing performance", align = "lcccc") %>%
  kable_classic(full_width = F) %>%
  kable_styling(bootstrap_options = c("hover", "condensed")) %>%
  pack_rows("Season: 2021/22", 1, 4) %>%
  pack_rows("Season: 2020/21", 5, 8) %>%
  pack_rows("Season: 2019/20", 9, 12) %>%
  pack_rows("Season: 2018/19", 13, 16) %>%
  column_spec(2, color = "white", bold = T,
              background = ifelse(epl_dat$spend < -100, "red","orange")) %>%
  column_spec(3, color = "white", bold = T,
              background = ifelse(epl_dat$pts > 80, "green","grey")) %>%
  column_spec(4, color = "white", bold = T,
              background = ifelse(epl_dat$gd > 50, "blue","skyblue"))  

# Add to code above using pipeto export and fit size
save_kable("Documents/R_Proj/Data/Liverpool/tab1.pdf")
  
## Game specific analysis over time
# Data - games performance (2017-2022)
s5 <- read_excel("Documents/R_Proj/Data/Liverpool/Season_2122.xlsx")
s4 <- read_excel("Documents/R_Proj/Data/Liverpool/Season_2021.xlsx")
s3 <- read_excel("Documents/R_Proj/Data/Liverpool/Season_1920.xlsx")
s2 <- read_excel("Documents/R_Proj/Data/Liverpool/Season_1819.xlsx")
s1 <- read_excel("Documents/R_Proj/Data/Liverpool/Season_1718.xlsx")

# Remove unnecessary columns and join data (all games 2017-2022)
s1a <- s1 %>%
  dplyr::select(-Venue,-Opponent)
s2a <- s2 %>%
  dplyr::select(-Venue,-Opponent)
s3a <- s3 %>%
  dplyr::select(-Venue,-Opponent,-Day)
s4a <- s4 %>%
  dplyr::select(-Venue,-Opponent)
s5a <- s5 %>%
  dplyr::select(-Venue,-Opponent)

s5_join <- bind_rows(s1a,s2a,s3a,s4a,s5a)

# Add column of xG over full season by seasons
s5_join <- s5_join %>%
  group_by(Season) %>%
  mutate(xG_total = sum(xG))

# Figure 4: violin for distributions of possessions data  
ggplot(s5_join, aes(x=Season,y=Poss, fill = Season, color = Season)) +
  geom_violin(width = 0.85, size = 0.25, alpha=0.5) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values = nba_palette("grizzlies")) +
  scale_color_manual(values = nba_palette("grizzlies")) +
  xlab("") + ylab("Posessions Metric") + ggtitle("Establishing possessions (2017-2022)") +
  theme_ipsum() +
  theme(legend.position = "none") +
  coord_flip()

# Reshape data (long) for comparing xG and xGA over time
s5a_join <- s5_join %>%
  dplyr::select(Season,xG,xGA,xG_total) %>%
  gather(x,values,xG:xGA,-Season) 

# Figure 5: xG/xGA distributions over time
ggplot(s5a_join) +
  geom_density_ridges(aes(x=values,y=Season,fill=x), scale = 2, rel_min_height = 0.01, alpha = 0.85) +
  geom_text(data=filter(s5a_join, Season=="2021/22"),
            aes(x=5.5, y=Season, label="Total"),
            color="black", size=3.5, vjust=-4) +
  geom_text(aes(x = 5.5, y = Season, label = xG_total), vjust = -0.75) +
  scale_fill_manual(values = c("#00AFBB","#FC4E07")) + labs(x=NULL, fill = "") + 
  theme_ridges(grid = T) + theme(legend.position = "top",
                                 legend.background = element_rect(size = 0.75, linetype = "solid", colour = "black"))

# Games data for all 5 years
s5b_join <- bind_rows(s1,s2,s3,s4,s5)

# Create groups for analysis
s5b_join <- s5b_join %>%
  mutate(cmp = ifelse(Opponent == "Chelsea" | Opponent == "Manchester Utd" | Opponent == "Manchester City","Top3","Other"))

# Add vars for counting number of wins/losses/draws
s5b_join$pts <- 0
s5b_join$pts[s5b_join$Result == "D"] <-1
s5b_join$pts[s5b_join$Result == "W"] <-3

# Sum number of wins/losses/draws per season
s5b_join <- s5b_join %>%
  mutate(win = ifelse(Result == "W",1,0),
         tie = ifelse(Result == "D",1,0),
         loss = ifelse(Result == "L",1,0))

s5c_join <- s5b_join %>%
  group_by(Season,cmp) %>%
  mutate(sumpts = cumsum(pts),
         sumw = cumsum(win),
         sumt = cumsum(tie),
         suml = cumsum(loss))

# Pull total number of wins/losses/draws per season
s5d_join <- s5c_join %>% 
  group_by(Season,cmp) %>% 
  summarise(pts_comp = max(sumpts),
            win_comp = max(sumw),
            tie_comp = max(sumt),
            loss_comp = max(suml)) %>%
  mutate(pts_prop = ifelse(cmp == "Top3",pts_comp/18,pts_comp/96),
         per = paste(win_comp,"W /",tie_comp,"D /",loss_comp,"L"))

# Figure 6: points gained by group and actual game results
ggplot(s5d_join, aes(x = Season, y=pts_prop)) +
  geom_bar(aes(fill=cmp),stat = "identity", position = position_dodge(1)) +
  geom_text(data = filter(s5d_join, cmp == "Other"),
    aes(label = paste(round(pts_prop*100,1),"%")),size=3,hjust=1.1,vjust=2.1,color = "white",fontface="bold") +
  geom_text(data = filter(s5d_join, cmp == "Top3"),
            aes(label = paste(round(pts_prop*100,1),"%")),size=3,hjust=1.1,vjust=-1.1,color = "white",fontface="bold") +
  geom_rect(aes(ymin=0.95, ymax=1.25, xmin=-Inf, xmax=Inf), fill="#FF0000") +
  geom_text(data=filter(s5d_join, cmp=="Other"),
            aes(label=per, x=Season, y=1.1), color = "white", fontface="bold", size=3.3,vjust=2) +
  geom_text(data=filter(s5d_join, cmp=="Top3"),
            aes(label=per, x=Season, y=1.1), color = "white", fontface="bold", size=3.3,vjust=-0.85) +
  geom_text(data=filter(s5d_join, Season=="2021/22"),
            aes(y=1.1, x=Season, label="Performance"),
            color="white", size=4, vjust=-3) +
  labs(fill = "Competition") +
  xlab("") + ylab("Proportion of points gained (of total by competition)") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(expand=c(0.22,0)) +
  scale_fill_manual(values = nba_palette("knicks_retro")) +
  coord_flip() + theme_bw() +
  theme(legend.position = "top",
        legend.background = element_rect(size = 0.75, linetype = "solid", colour = "black"))

