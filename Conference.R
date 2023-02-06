library(tidyverse)
library(ggplot2)
library(ggimage)
library(gsheet)
rm(list = ls())

women <- FALSE
date <- "January 29, 2023"
n <- 7
uaa <- TRUE

mbb_logs <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/mbb_uaa_scout.csv")
wbb_logs <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/wbb_uaa_scout.csv")
mbb_games <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1BcIP7CIYDTNnedcRG3U3HAIwluaJtz8LCAKfArlEh98/edit#gid=136251738")
wbb_games <- gsheet2tbl("https://docs.google.com/spreadsheets/d/12JWqAMfVrSZobLohmxQK6PyrbNbOQbM6ux4LxXqWenM/edit#gid=1703250336")
logos <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/uaa_logos.csv")

if (women) {
  logs <- wbb_logs
  games <- wbb_games
  if (uaa) {
    subtitle <- paste("UAA Women's Basketball | Through", date)
  } else {
    subtitle <- paste("UAA Women's Basketball | Through", date, "| All Games")
  }
} else {
  logs <- mbb_logs
  games <- mbb_games
  if (uaa) {
    subtitle <- paste("UAA Men's Basketball | Through", date)
  } else {
    subtitle <- paste("UAA Men's Basketball | Through", date, "| All Games")
  }
}

if (uaa) {
  conference <- "UAA Total"
} else {
  conference <- "Total"
}

logs <-
  logs %>%
  filter(Opponent == conference) %>%
  select(Team, `PPP`, `OPP PPP`, `ORB%`, `DRB%`, `TO%`, `OPP TO%`)
games <-
  games %>%
  filter(Game == conference) %>%
  mutate(Team = "Chicago") %>%
  select(Team, `PPP`, `OPP PPP`, `ORB%`, `DRB%`, `TO%`, `OPP TO%`)

conference <- 
  rbind(logs, games) %>%
  merge(logos, 
        by="Team")

ppp <-
  conference %>%
  ggplot(aes(x=`PPP`,
             y=`OPP PPP`)) +
  geom_hline(yintercept=mean(conference$`OPP PPP`), linetype="dashed") +
  geom_vline(xintercept=mean(conference$`PPP`), linetype="dashed") +
  geom_image(aes(image=URL),
             size=0.1,
             stat='identity') +
  labs(title="Who runs the most efficient offense/defense?",
       subtitle=subtitle,
       x="Points Per Possession (PPP)",
       y="Opponent Points Per Possession (OPP PPP)",
       caption=paste("(", toString(n), " total games)", sep="")) +
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

rebounds <-
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
       y="Offensive Rebound Rate (ORB%)",
       caption=paste("(", toString(n), " total games)", sep="")) +
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

turnovers <- 
  conference %>%
  ggplot(aes(x=`TO%`,
             y=`OPP TO%`)) +
  geom_hline(yintercept=mean(conference$`TO%`), linetype="dashed") +
  geom_vline(xintercept=mean(conference$`OPP TO%`), linetype="dashed") +
  geom_image(aes(image=URL),
             size=0.1,
             stat='identity') +
  xlim(floor(min(conference$`TO%`)), ceiling(max(conference$`TO%`))) +
  ylim(floor(min(conference$`OPP TO%`)), ceiling(max(conference$`OPP TO%`))) +
  labs(title="Which teams commit and force the most turnovers?",
       subtitle=subtitle,
       x="Team Turnover Rate (TO%)",
       y="Opponent Turnover Rate (OPP TO%)",
       caption=paste("(", toString(n), " total games)", sep="")) +
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

# ppp
rebounds
# turnovers
