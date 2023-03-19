library(tidyverse)
library(ggplot2)
library(ggimage)
library(gsheet)
rm(list = ls())

setwd("/Users/jeremydumalig/Documents/GitHub/Chicago-Analytics")
  
ncaa <- 
  merge(read_csv("ncaa_stats.csv"), 
        read_csv("ncaa_logos.csv"), 
        by="TEAM") %>%
  merge(read_csv("tournament_links.csv"),
        by="LINK") %>%
  mutate(`FTA%` = 100 * round(FTA / FGA, 3))

ncaa %>%
  ggplot(aes(x=`FTA%`,
             y=`FT%`)) +
  geom_hline(yintercept=mean(ncaa$`FT%`), linetype="dashed") +
  geom_vline(xintercept=mean(ncaa$`FTA%`), linetype="dashed") +
  geom_image(aes(image=Logo),
             size=0.1,
             stat='identity') +
  xlim(floor(min(ncaa$`FTA%`)), ceiling(max(ncaa$`FTA%`))) +
  ylim(floor(min(ncaa$`FT%`)), ceiling(max(ncaa$`FT%`))) +
  labs(title="Which teams shoot the most free throws?",
       subtitle="NCAA Division I Men's Basketball | 2022-23 Season",
       x="Free Throw Rate (FTA%)",
       y="Free Throw Percentage (FT%)") +
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90",
                                   color = "black"),
    legend.box.background = element_rect(size=0.75),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    plot.title = element_text(size=18,
                              face="bold"),
    plot.subtitle = element_text(size=14),
    plot.caption = element_text(size=10))
