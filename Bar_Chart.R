library(tidyverse)
library(ggplot2)
library(gsheet)
library(ggrepel)
library(png)
library(gt)
rm(list=ls())

setwd("~/Documents/Portfolio/Heat Check")

raw_shots <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1w0Aq8FsDzdTbSLhpm2HvSffiIRuZEM8zBTmIpNG7Kxs/edit?usp=sharing")
players <- levels( factor(raw_shots$Player) )

shots <- raw_shots %>%
  group_by(Player, Region) %>%
  summarise(FGM = sum(Outcome),
            FGA = n()) %>%
  mutate(`FG%` = 100 * round(FGM / FGA, 3),
         Splits = paste(paste(as.character(FGM), 
                              as.character(FGA), 
                              sep="/"), 
                        paste(as.character(100*round(FGM/FGA, 3)),
                              "%",
                              sep=""), 
                        sep="\n"),
         Region = case_when(
           Region == 'P' ~ "Paint",
           Region == 'LC' ~ "Left Corner",
           Region == 'RC' ~ "Right Corner",
           Region == 'LW' ~ "Left Wing",
           Region == 'RW' ~ "Right Wing",
           Region == 'T' ~ "Top",
           Region == 'TK' ~ "Top of the Key",
           Region == 'RE' ~ "Right Elbow",
           Region == 'LE' ~ "Left Elbow",
           Region == 'RB' ~ "Right Baseline",
           Region == 'LB' ~ "Left Baseline"
         )) %>%
  arrange(desc(FGA)) %>%
  ungroup()

subtitle <- "Foreign Tour Practices"

shot_bar <- function(player, percentage=FALSE) {
  if (player != 'All') {
    df <- filter(shots, Player == player)
  } else {
    df <- shots %>% 
      group_by(Region) %>% 
      summarize(FGM = sum(FGM), 
                FGA = sum(FGA)) %>%
      mutate(`FG%` = 100 * round(FGM / FGA, 3)) %>%
      ungroup()
  }
  
  title <- "All Players"
  if (player != 'All') {
    title <- player
  }
  
  y_label <- 'Field Goal Attempts (FGA)'
  if (percentage) {
    df <- mutate(df, FGA = `FG%`)
    y_label <- 'Field Goal Percentage (FG%)'
  }
  
  ggplot(data=df) + 
    geom_bar(aes(x=fct_rev(fct_reorder(Region, FGA)), 
                 y=FGA,
                 fill=FGA),
             color='black',
             stat="identity",
             show.legend=FALSE) +
    scale_fill_gradient(low="#c8a4a2",
                        high="#7b0001") +
    labs(x='',
         y=y_label,
         title=title,
         subtitle=subtitle) +
    coord_flip() +
    theme_linedraw() +
    theme(
      plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
      plot.background = element_rect(
        fill = "grey90",
        color = "black"
      ),
      legend.box.background = element_rect(size=0.75),
      legend.title = element_text(size=10),
      axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15),
      plot.title = element_text(face="bold",
                                size=20),
      plot.subtitle = element_text(size=16)
    )
}

shot_bar("Josh Preston", percentage=FALSE)
