library(tidyverse)
library(ggplot2)
library(ggimage)
library(gsheet)
rm(list = ls())

setwd("/Users/jeremydumalig/Documents/GitHub/Chicago-Analytics")

women <- TRUE
date <- "February 25, 2023"
n <- 14
uaa <- TRUE

mbb_logs <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/mbb_uaa_scout.csv")
wbb_logs <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/wbb_uaa_scout.csv")
mbb_games <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1BcIP7CIYDTNnedcRG3U3HAIwluaJtz8LCAKfArlEh98/edit#gid=136251738")
wbb_games <- gsheet2tbl("https://docs.google.com/spreadsheets/d/12JWqAMfVrSZobLohmxQK6PyrbNbOQbM6ux4LxXqWenM/edit#gid=1703250336")
logos <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/uaa_logos.csv")
mbb_standings <- read_csv("uaa_mbb_standings14.csv") %>% mutate(Ranking = 9 - Ranking)
wbb_standings <- read_csv("uaa_wbb_standings14.csv") %>% mutate(Ranking = 9 - Ranking)
unw <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/unw_scout.csv") %>% 
  mutate(Team = "UNW-St. Paul") %>%
  filter(Opponent == "Total")
claire <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/uweauclaire_scout.csv") %>% 
  mutate(Team = "UW-Eau Claire") %>%
  filter(Opponent == "Total")
whitman <- read_csv("https://raw.githubusercontent.com/jeremydumalig/Chicago-Analytics/main/whitman_scout.csv") %>% 
  mutate(Team = "Whitman") %>%
  filter(Opponent == "Total")

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

logs_conf <-
  logs %>%
  filter(Opponent == conference)
games_conf <-
  games %>%
  filter(Game == conference) %>%
  mutate(Team = "Chicago")

conference <- 
  rbind(select(logs_conf, Team, `PPP`, `OPP PPP`, `ORB%`, `DRB%`, `TO%`, `OPP TO%`), 
        select(games_conf, Team, `PPP`, `OPP PPP`, `ORB%`, `DRB%`, `TO%`, `OPP TO%`)) %>%
  rbind(select(unw, Team, `PPP`, `OPP PPP`, `ORB%`, `DRB%`, `TO%`, `OPP TO%`)) %>%
  rbind(select(claire, Team, `PPP`, `OPP PPP`, `ORB%`, `DRB%`, `TO%`, `OPP TO%`)) %>%
  rbind(select(whitman, Team, `PPP`, `OPP PPP`, `ORB%`, `DRB%`, `TO%`, `OPP TO%`)) %>%
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

mbb_rankings <-
  mbb_standings %>%
  ggplot(aes(x=Game, y=Ranking)) +
  geom_point(aes(group=Team, color=Team),
             size=3,
             alpha=0.5,
             show.legend=FALSE) +
  geom_line(aes(group=Team, color=Team),
            size=1,
            show.legend=FALSE) +
  geom_image(data=filter(mbb_standings, Game == n),
             aes(image=URL),
             size=0.075,
             stat='identity') +
  scale_color_manual(values=c("#19374c", "#bd1c40", "#e7a612", 
                                       "#661b8f", "#c61c31", 
                                       "#ffdc04", "#1b4264", "#870f00"),
                     breaks = arrange(filter(mbb_standings, Game == n), desc(Ranking))$Team) +
  scale_x_discrete(limits = levels(wbb_standings$Game)) +
  scale_y_discrete(limits = rev(levels(wbb_standings$Ranking))) +
  labs(title="2023 UAA Men's Basketball Standings",
       x=paste("Games Played (", n, " total)", sep=""),
       y="Wins",
       caption="NCAA Tournament Bids: Case Western, WashU, Emory, NYU, Rochester") +
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90",
                                   color = "black"),
    legend.box.background = element_rect(size=0.75),
    axis.title.x = element_text(size=18,
                                margin = margin(t=10, b=5)),
    axis.title.y = element_text(size=18,
                                margin = margin(r=10)),
    plot.title = element_text(size=24,
                              face="bold",
                              margin = margin(b=10)))
wbb_rankings <-
  wbb_standings %>%
  filter(Game > 0) %>%
  ggplot(aes(x=Game, y=Ranking)) +
  geom_point(aes(group=Team, color=Team),
             size=3,
             alpha=0.75,
             show.legend=FALSE) +
  geom_line(aes(group=Team, color=Team),
            size=1,
            alpha=0.5,
            show.legend=FALSE) +
  geom_image(data=filter(wbb_standings, Game == n),
             aes(image=URL),
             size=0.075,
             stat='identity') +
  scale_color_manual(values=c("#661b8f", "#870f00", "#bd1c40", 
                                       "#e7a612", "#ffdc04", 
                                       "#19374c", "#c61c31", "#1b4264"),
                                       breaks = arrange(filter(wbb_standings, Game == n), desc(Ranking))$Team) +
  scale_x_discrete(limits = levels(wbb_standings$Game)) +
  scale_y_discrete(limits = rev(levels(wbb_standings$Ranking))) +
  labs(title="2023 UAA Women's Basketball Standings",
       x=paste("Games Played (", n, " total)", sep=""),
       y="Ranking",
       caption="NCAA Tournament Bids: NYU, Chicago, WashU, Emory") +
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90",
                                   color = "black"),
    legend.box.background = element_rect(size=0.75),
    axis.title.x = element_text(size=18,
                                margin = margin(t=10, b=5)),
    axis.title.y = element_text(size=18,
                                margin = margin(r=10)),
    plot.title = element_text(size=24,
                              face="bold",
                              margin = margin(b=10)))

# ppp
# rebounds
# turnovers
mbb_rankings
wbb_rankings
