setwd("~/Downloads/UAA")

library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
# library("av")
rm(list=ls())

title <- "UChicago at Colorado College"
filename <- "colorado"
away <- "Chicago"
home <- "Colorado"
c_away <- "#810100"
c_home <- "#F7AE01"

# 
game <- read_csv(file=paste(filename, ".csv", sep=""))

# 
gg_game <- ggplot(game, aes(x=TEAM, y=SCORE, fill=TEAM)) +
  geom_bar(stat='identity',
           show.legend=FALSE) + 
  scale_fill_manual(values=c(c_away, c_home)) +
  scale_x_discrete(labels = c(away, home)) +
  labs(title=title,
       x="{game[frame,]$Score}",
       y="Score",
       subtitle="{game[frame,]$Lineup}") +
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(
      fill = "grey90",
      color = "black"
    ),
    legend.background = element_rect(color="black"),
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=10),
    legend.title = element_text(size=10),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  )

# 
animate(gg_game + 
          transition_states(`...1`, 
                            transition_length=3, 
                            state_length=1) + 
          ease_aes('sine-in-out'), 
        nframes=nrow(game) + 15,
        end_pause=15,
        duration=30)

#
anim_save(paste(filename, ".gif", sep=""))
