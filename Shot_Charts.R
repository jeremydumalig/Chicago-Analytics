library(tidyverse)
library(ggplot2)
library(gsheet)
library(ggrepel)
library(ggnewscale)
library(ggpubr)
library(png)
library(gt)
library(webshot)
library(showtext)
library(kableExtra)
rm(list=ls())

options(dplyr.summarise.inform = FALSE)

setwd("~/Documents/Portfolio/UC MBB")

men <- TRUE
game_status <- FALSE

mbb_url <- "https://docs.google.com/spreadsheets/d/1BcIP7CIYDTNnedcRG3U3HAIwluaJtz8LCAKfArlEh98/edit#gid=2032963821"
wbb_url <- "https://docs.google.com/spreadsheets/d/12JWqAMfVrSZobLohmxQK6PyrbNbOQbM6ux4LxXqWenM/edit#gid=2032963821"

mbb_game_dates <- c("11/10/22", "11/12/22", "11/13/22", "11/18/22", "11/19/22", "11/26/22", "11/30/22", "12/11/22", "12/14/22", "12/20/22")
wbb_game_dates <- c("11/12/22", "11/13/22", "11/18/22", "11/19/22", "11/22/22", "11/27/22", "11/30/22", "12/3/22", "12/14/22", "12/18/22")
players <- c('Alec Bryan', 'Kerem Ozturk', 'Leonardo Edo', 'Skyler Twyman', 'Ezra Moos', 'Thomas Kurowski', 'Dashiel Walker', 'Elliot Paschal', 'Arrish Bhandal', 'Josh Preston', 'Jackson Piotrowski', 'Bryce Hopkins', 'Tola Olorode', 'Joe Berry', 'Ben Chasin', 'Blake Hawthorne', 'Eamonn Kenah', 'Alex Battist', 'Ellie Gross', 'Kate Gross', 'Grace Hynes', 'Sophia North', 'Ashley Gao', 'Isabelle Herrera', 'Bella Alfaro', 'Lindsey Carter', 'Caroline Workman', 'Mallory Brodnik', 'Peyton Van Soest', 'Alexis Clark', 'Amber Williams', 'Marissa Powe')
teams <- c('Lake Forest', 'Albion', 'North Central', "Mount St. Joe's", 'Benedictine', 'Colorado College', 'North Park', 'Colby', 'Carroll', 'Wheaton', 'Carthage', 'Eureka', 'Kalamazoo', 'Edgewood', 'Whitewater', 'Illinois Tech', 'Lutheran', 'Wesleyan')

if (men) {
  team_url <- mbb_url
  game_dates <- mbb_game_dates
} else {
  team_url <- wbb_url
  game_dates <- wbb_game_dates
}

raw_shots <- gsheet2tbl(team_url) %>%
  mutate(Region = case_when(
    Region == 'P' ~ "Paint",
    Region == 'LC' ~ "Left Corner",
    Region == 'RC' ~ "Right Corner",
    Region == 'LW' ~ "Left Wing",
    Region == 'RW' ~ "Right Wing",
    Region == 'T' ~ "Top",
    Region == 'TK' ~ "Top of Key",
    Region == 'RE' ~ "Right Elbow",
    Region == 'LE' ~ "Left Elbow",
    Region == 'RB' ~ "Right Baseline",
    Region == 'LB' ~ "Left Baseline"
  ),
  Range = case_when(
    (Region == 'Left Corner') | 
      (Region == 'Left Wing') | 
      (Region == 'Top') | 
      (Region == 'Right Wing') | 
      (Region == 'Right Corner') ~ "3",
    Region == 'Paint' ~ "Paint",
    TRUE ~ "2"
  ),
  `Shot Type` = case_when(
    `Shot Type` == 'S' ~ "Catch & Shoot (Set)",
    `Shot Type` == 'M' ~ "Catch & Shoot (Moving)",
    `Shot Type` == 'L' ~ "Layup",
    `Shot Type` == 'F' ~ "Floater",
    `Shot Type` == 'H' ~ "Hook Shot",
    `Shot Type` == 'B' ~ "Stepback",
    `Shot Type` == 'P' ~ "Pull-Up"
  ),
  Game = Date %in% game_dates) %>%
  filter(Game == game_status)

players <- levels( factor(raw_shots$Player) )

court <- read_csv(file="court.csv") %>%
  select(X, Y, Region) %>%
  filter(Region != 'Out of Bounds')
region_labels <- read_csv(file="region_labels.csv") %>%
  select(Region, X, Y)

player_region <- function(df) {
  df %>%
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
                          sep="\n")) %>%
    ungroup()
}
player_type <- function(df) {
  df %>%
    na.omit() %>%
    group_by(Player, `Shot Type`) %>%
    summarise(FGM = sum(Outcome),
              FGA = n()) %>%
    mutate(`FG%` = 100 * round(FGM / FGA, 3),
           Splits = paste(paste(as.character(FGM), 
                                as.character(FGA), 
                                sep="/"),
                          paste(as.character(100*round(FGM/FGA, 3)),
                                "%",
                                sep=""), 
                          sep="\n")) %>%
    ungroup()
}
team_region <- function(df) {
  df %>%
    group_by(Region) %>%
    summarise(FGM = sum(Outcome),
              FGA = n()) %>%
    mutate(Player = "Total",
           `FG%` = 100 * round(FGM / FGA, 3),
           Splits = paste(paste(as.character(FGM), 
                                as.character(FGA), 
                                sep="/"), 
                          paste(as.character(100*round(FGM/FGA, 3)),
                                "%",
                                sep=""), 
                          sep="\n")) %>%
    ungroup()
}
team_type <- function(df) {
  df %>%
    na.omit() %>%
    group_by(`Shot Type`) %>%
    summarise(FGM = sum(Outcome),
              FGA = n()) %>%
    mutate(Player = "Total",
           `FG%` = 100 * round(FGM / FGA, 3),
           Splits = paste(paste(as.character(FGM), 
                                as.character(FGA), 
                                sep="/"), 
                          paste(as.character(100*round(FGM/FGA, 3)),
                                "%",
                                sep=""), 
                          sep="\n")) %>%
    ungroup()
}

shot_map <- function(player, shot_type, title, subtitle) {
  df <- raw_shots
  if (shot_type != "All") {
    df <- filter(raw_shots, `Shot Type` == shot_type)
  }
  
  if (player == 'All') {
    df <- team_region(df)
  } else {
    df <- player_region( filter(df, Player == player) )
  }
  
  shots <- merge(x=court,
                 y=select(df, Player, Region, FGA, `FG%`, Splits),
                 by="Region",
                 all.x=TRUE)
  labels <- merge(x=region_labels,
                  y=df,
                  by="Region",
                  all.x=TRUE)
  
  if (player != 'All') {
    title <- player
  }
  if (shot_type != 'All') {
    title <- paste(title, shot_type, sep=" - ")
  }
  
  plot <- 
    ggplot(data=shots, aes(x=X, 
                           y=Y, 
                           color=`FG%`)) + 
    geom_point(shape=15,
               size=2.5,
               show.legend=FALSE) +
    scale_color_gradient(low="white", 
                      high="red",
                      limits=range(max(0, 
                                       min(range(drop_na(shots)$`FG%`)) - 5),
                                   min(100, 
                                       max(range(drop_na(shots)$`FG%`)) + 20)),
                      na.value="#c4c4c4") +
    annotation_raster(readPNG("court.png"), 
                      ymin = 0,
                      ymax= 47,
                      xmin = 0,
                      xmax = 50) +
    new_scale('alpha') +
    geom_label(
      data=labels, 
      aes(x=X, y=Y, label=Splits),
      size=4,
      color='black',
      fill = alpha(c("white"), 0.5)) +
    xlim(0, 50) +
    ylim(0, 47) +
    labs(x="", 
         y="",
         title=md(title),
         subtitle=subtitle) +
    theme_linedraw() +
    theme(
      plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
      plot.background = element_rect(
        fill = "grey90",
        color = "black"),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.title = element_text(face="bold",
                                size=20),
      plot.subtitle = element_text(size=16),
      legend.title = element_text(size=10)
    )
  
  return(plot)
}
region_bar <- function(player, percentage=FALSE, title, subtitle) {
  if (player != 'All') {
    df <- player_region( filter(raw_shots, Player == player) )
  } else {
    df <- team_region(raw_shots) %>% 
      group_by(Region) %>% 
      summarize(FGM = sum(FGM), 
                FGA = sum(FGA)) %>%
      mutate(`FG%` = 100 * round(FGM / FGA, 3)) %>%
      ungroup()
  }
  
  if (player != 'All') {
    title <- player
  }
  
  y_label <- 'Field Goal Attempts (FGA)'
  if (percentage) {
    df <- mutate(df, FGA = `FG%`)
    y_label <- 'Field Goal Percentage (FG%)'
  }
  
  region_plot <-
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
         title=paste(title, "(Region)", sep=" "),
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
  
  return(region_plot)
}
type_bar <- function(player, percentage=FALSE, title, subtitle) {
  if (player != 'All') {
    df <- player_type( filter(raw_shots, Player == player) )
  } else {
    df <- team_type(raw_shots) %>% 
      group_by(`Shot Type`) %>% 
      summarize(FGM = sum(FGM), 
                FGA = sum(FGA)) %>%
      mutate(`FG%` = 100 * round(FGM / FGA, 3)) %>%
      ungroup()
  }
  
  if (player != 'All') {
    title <- player
  }
  
  y_label <- 'Field Goal Attempts (FGA)'
  if (percentage) {
    df <- mutate(df, FGA = `FG%`)
    y_label <- 'Field Goal Percentage (FG%)'
  }
  
  type_plot <-
    ggplot(data=df) + 
    geom_bar(aes(x=fct_rev(fct_reorder(`Shot Type`, FGA)), 
                 y=FGA,
                 fill=FGA),
             color='black',
             stat="identity",
             show.legend=FALSE) +
    scale_fill_gradient(low="#c8a4a2",
                        high="#7b0001") +
    labs(x='',
         y=y_label,
         title=paste(title, "(Shot Type)", sep=" "),
         subtitle=subtitle) +
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
  
  return(type_plot)
}

shot_map_table <- function(player, shot_type, title, subtitle) {
  df <- raw_shots
  if (shot_type != "All") {
    df <- filter(raw_shots, `Shot Type` == shot_type)
  }
  
  if (player == 'All') {
    df <- team_region(df)
  } else {
    df <- player_region( filter(df, Player == player) )
  }
  
  if (player != 'All') {
    title <- player
  }
  if (shot_type != 'All') {
    title <- paste(title, shot_type, sep=" - ")
  }
  
  table <- df %>%
    mutate(Splits = paste(as.character(FGM), 
                          as.character(FGA), 
                          sep="/")) %>%
    select(Region, Splits, `FG%`) %>%
    arrange(desc(`FG%`)) %>%
    gt() %>%
    tab_header( 
      title = md(paste("**", title, " (Region)**", 
                       sep="")),
      subtitle = subtitle)
  
  return(table)
}
region_shot_table <- function(player, shot_type, title, subtitle) {
  df <- na.omit(raw_shots)
  
  if (player != 'All') {
    df <- filter(df, Player == player)
  }
  if (shot_type != 'All') {
    df <- filter(df, `Shot Type` == shot_type)
  }
  
  player_shots <- 
    df %>%
    group_by(Region, `Shot Type`) %>%
    summarise(FGM = sum(Outcome),
              FGA = n()) %>%
    mutate(`FG%` = 100 * round(FGM / FGA, 3),
           Splits = paste(paste(as.character(FGM), 
                                as.character(FGA), 
                                sep="/"))) %>%
    arrange(Region, `Shot Type`, desc(`FG%`)) %>%
    ungroup()
  
  if (player != 'All') {
    title <- player
  }
  
  player_table <- player_shots %>%
    select(Region, `Shot Type`, Splits, `FG%`) %>%
    gt() %>%
    tab_header(
      title = md(paste("**", title, " (Region + Shot Type)**", sep="")),
      subtitle = subtitle)
  
  return(player_table)
}
shot_table <- function(player, shot_type, title, subtitle) {
  df <- na.omit(raw_shots)
  
  if (player != 'All') {
    df <- filter(df, Player == player)
  }
  if (shot_type != 'All') {
    df <- filter(df, `Shot Type` == shot_type)
  }
  
  player_shots <- 
    df %>%
    group_by(`Shot Type`) %>%
    summarise(FGM = sum(Outcome),
              FGA = n()) %>%
    mutate(`FG%` = 100 * round(FGM / FGA, 3),
           Splits = paste(paste(as.character(FGM), 
                                as.character(FGA), 
                                sep="/"))) %>%
    arrange(`Shot Type`, desc(`FG%`)) %>%
    ungroup()
  
  if (player != 'All') {
    title <- player
  }
  
  player_table <- player_shots %>%
    select(`Shot Type`, Splits, `FG%`) %>%
    gt() %>%
    tab_header(
      title = md(paste("**", title, " (Shot Type)**", sep="")),
      subtitle = subtitle)
  
  return(player_table)
}
region_table <- function(region, shot_type, subtitle) {
  if (shot_type != 'All') {
    df <- filter(raw_shots, `Shot Type` == shot_type)
  } else {
    df <- raw_shots
  }
  
  df <- df %>%
    group_by(Region, Player) %>%
    summarize(FGM = sum(Outcome),
              FGA = n(),
              `FG%` = 100 * round(FGM / FGA, 3)) %>%
    ungroup() %>%
    arrange(Region, desc(`FG%`))
  
  if (region != 'All') {
    df <- df %>%
      filter(Region == region) %>%
      select(Player, FGM, FGA, `FG%`)
    title <- region
  } else {
    df <- select(df, Region, Player, FGM, FGA, `FG%`)
    title <- "All Regions"
  }
  
  table <- df %>%
    gt() %>%
    tab_header( 
      title = md(paste("**", title, "**", sep="")),
      subtitle = subtitle)
  
  return(table)
}
range_table <- function(shot_type, range, subtitle) {
  if (shot_type != 'All') {
    df <- filter(raw_shots, `Shot Type` == shot_type)
    title2 <- paste(": ", shot_type, sep="")
  } else {
    df <- raw_shots
    title2 <- ""
  }
  
  if (range != 'All') {
    df <- filter(df, Range == range)
    
    if (range != 'Paint') {
      title1 <- paste(range, "-Point Range", sep="")
    } else {
      title1 <- "Paint"
    }
  } else {
    title1 <- "All Shots"
  }
  
  table <- df %>%
    group_by(Player) %>%
    summarize(FGM = sum(Outcome),
              FGA = n(),
              `FG%` = 100 * round(FGM / FGA, 3)) %>%
    ungroup() %>%
    arrange(desc(`FG%`)) %>%
    select(Player, FGM, FGA, `FG%`) %>%
    gt() %>%
    tab_header( 
      title = md(paste("**", title1, title2, "**", sep="")),
      subtitle = subtitle)
  
  return(table)
}
player_range_table <- function(player, shot_type, range, subtitle) {
  if (shot_type != 'All') {
    df <- filter(raw_shots, `Shot Type` == shot_type)
    title2 <- paste(": ", shot_type, sep="")
  } else {
    df <- raw_shots
    title2 <- ""
  }
  
  table <- 
    df %>%
    filter(Player == player) %>%
    group_by(Range) %>%
    summarize(FGM = sum(Outcome),
              FGA = n(),
              `FG%` = 100 * round(FGM / FGA, 3)) %>%
    ungroup() %>%
    arrange(desc(`FG%`)) %>%
    mutate(Range = case_when(
      Range == "2" ~ "2-Point Range",
      Range == "3" ~ "3-Point Range",
      TRUE ~ "Paint"
    )) %>%
    gt() %>%
    cols_label(
      Range = ""
    ) %>%
    tab_header( 
      title = md(paste("**", player, title2, " (Range)**", sep="")),
      subtitle = subtitle)
  
  return(table)
}

title <- 'UChicago'
# subtitle <- 'UChicago at Lutheran | December 18, 2022'
subtitle <- "University of Chicago Men's Basketball | 2022 Preseason Practices"
player <- 'All'
region <- 'All'
shot_type <- 'All'
range <- 'All'

# raw_shots <- filter(raw_shots, Date == "12/18/22", Player != "Lutheran")
shot_map(player, shot_type, title, subtitle)

for (player in players) {
  shot_map(player, shot_type, title, subtitle)
  ggsave(paste(strsplit(strsplit(player, " ")[[1]][1], "")[[1]][1],
               strsplit(player, " ")[[1]][2], 
               ".png", 
               sep=""),
         width = 2880,
         height = 1800,
         units = "px")
}
