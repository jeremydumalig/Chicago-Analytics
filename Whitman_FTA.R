library(tidyverse)
library(ggplot2)
library(ggimage)
rm(list = ls())

setwd("/Users/jeremydumalig/Documents/GitHub/Chicago-Analytics")

post_whitman <- read_csv("post_whitman_fta.csv")

post_whitman %>%
  ggplot(aes(x=`FTA%`, y=`OPP FTA%`)) +
  geom_hline(yintercept=mean(post_whitman$`OPP FTA%`),
             linetype="dashed") +
  geom_vline(xintercept=mean(post_whitman$`FTA%`),
             linetype="dashed") +
  annotate("rect", 
           xmin=57, xmax=69, ymin=8.4, ymax=19.4,
           alpha=0.25) +
  annotate("text", 
           x=63, y=17.9, 
           label="3/4/23 vs Whitman",
           fontface="bold") +
  geom_image(aes(image=URL),
             size=0.1,
             stat='identity') +
  scale_y_reverse() +
  labs(title="How Often Is UChicago Shooting Free Throws?",
       subtitle="University of Chicago Women's Basketball | Through March 4, 2023 (27 games)",
       x="Free Throw Attempt Rate (FTA%)",
       y="Opponent Free Throw Attempt Rate (FTA%)",
       caption="Note: the y-axis is reversed to place games\nwith high FTA% differentials in top-right region") +
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90",
                                   color = "black"),
    legend.box.background = element_rect(size=0.75),
    axis.title.x = element_text(size=14,
                                margin=margin(t=5,
                                              b=5)),
    axis.title.y = element_text(size=14,
                                margin=margin(r=5)),
    plot.title = element_text(size=18,
                              face="bold"),
    plot.subtitle = element_text(size=14),
    plot.caption = element_text(size=10))
