library(tidyverse)
library(ggplot2)
library(ggimage)
library(gsheet)
rm(list = ls())

women <- TRUE

mbb_logs <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/mbb_uaa_scout.csv")
wbb_logs <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/wbb_uaa_scout.csv")
mbb_games <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1BcIP7CIYDTNnedcRG3U3HAIwluaJtz8LCAKfArlEh98/edit#gid=136251738")
wbb_games <- gsheet2tbl("https://docs.google.com/spreadsheets/d/12JWqAMfVrSZobLohmxQK6PyrbNbOQbM6ux4LxXqWenM/edit#gid=1703250336")
logos <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/uaa_logos.csv")

if (women) {
  logs <- wbb_logs
  games <- wbb_games
  subtitle <- "UAA Women's Basketball | Through December 30, 2022"
} else {
  logs <- mbb_logs
  games <- mbb_games
  subtitle <- "UAA Men's Basketball | Through December 30, 2022"
}

logs <-
  logs %>%
  filter(Opponent == "Total") %>%
  select(Team, `ORB%`, `DRB%`)
games <-
  games %>%
  filter(Game == "Total") %>%
  mutate(Team = "Chicago") %>%
  select(Team, `ORB%`, `DRB%`)

conference <- 
  rbind(logs, games) %>%
  merge(logos, 
        by="Team")

conference %>%
  ggplot(aes(x=`DRB%`,
             y=`ORB%`)) +
  geom_hline(yintercept=mean(conference$`ORB%`), linetype="dashed") +
  geom_vline(xintercept=mean(conference$`DRB%`), linetype="dashed") +
  geom_image(aes(image=URL),
             size=0.1,
             stat='identity') +
  labs(title="Who are the best and worst rebounding teams?",
       subtitle=subtitle,
       x="Defensive Rebound Rate (DRB%)",
       y="Offensive Rebound Rate (ORB%)") +
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