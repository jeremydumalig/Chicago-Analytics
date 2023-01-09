library(tidyverse)
library(ggplot2)
library(gsheet)
library(gt)
rm(list = ls())

setwd("/Users/jeremydumalig/Documents/GitHub/Chicago-Analytics")

traditional_averages <- read_csv(file = "season_averages23.csv")
advanced_averages <- read_csv(file = "advanced_averages23.csv")

####################

traditional_averages <- 
  select(traditional_averages,
         PTS, FGA, `3PA`, FTA, `FG%`, `3P%`, `FG%`,
         AST, REB, OREB, DREB, STL, BLK, PF,
         TOV,
         `PTS OFF TOV`,
         `2ND CHANCE`,
         `PAINT`,
         `FASTBREAK`,
         BENCH)

####################

differential_sign <- function(number) {
  if (number == 0) {
    return("")
  }
  
  if (substr(format(number, nsmall = 1), 1, 1) != "-") {
    return(paste("+", format(number, nsmall = 1), sep = ""))
  } else {
    return(paste("-", format(abs(number), nsmall = 1), sep = ""))
  }
}

pregame_traditional <-
  function(dataframe, type, opponent, location, date) {
    if (location == 'Home') {
      sub <- paste(date, ' | ', opponent, ' at UChicago', sep = '')
    } else {
      sub <- paste(date, ' | UChicago at ', opponent, sep = '')
    }
    
    dataframe <-
      dataframe %>%
      t() %>%
      data.frame() %>%
      mutate(Difference = as.numeric(X2) - as.numeric(X1))
    dataframe %>%
      mutate(Difference = map(dataframe$Difference, differential_sign)) %>%
      select(X3, X1, Difference, X2) %>%
      gt() %>%
      cols_label(
        X3 = '',
        X1 = 'Chicago',
        Difference = '',
        X2 = 'Opponent'
      ) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = X3)) %>%
      tab_style(style = list(cell_text(style = "italic", size = "small")),
                locations = cells_body(columns = Difference)) %>%
      tab_header(title = md(paste('**Pre-Game Report: ', type, '**', sep =
                                    '')),
                 subtitle = sub)
  }
pregame_advanced <- 
  function(dataframe, opponent, location, date) {
    if (location == 'Home') {
      sub <- paste(date, ' | ', opponent, ' at UChicago', sep = '')
    } else {
      sub <- paste(date, ' | UChicago at ', opponent, sep = '')
    }
    
    dataframe <-
      dataframe %>%
      select(-`...1`,-Team) %>%
      t() %>%
      data.frame() %>%
      mutate(Difference = case_when(
        (X3 == "PTS DIFF PER40") ~ 0,
        TRUE ~ as.numeric(X2) - as.numeric(X1)
      ))
    dataframe %>%
      mutate(Difference = map(dataframe$Difference, differential_sign)) %>%
      select(X3, X1, Difference, X2) %>%
      gt() %>%
      cols_label(
        X3 = '',
        X1 = 'Chicago',
        Difference = '',
        X2 = 'Opponent'
      ) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = X3)) %>%
      tab_style(style = list(cell_text(style = "italic", size = "small")),
                locations = cells_body(columns = Difference)) %>%
      tab_header(title = md('**Pre-Game Report: Advanced**'),
                 subtitle = sub)
  }

####################

info <- c('WashU', 'Away', '1/7/23')

pregame_traditional(traditional_averages, 'Traditional', info[1], info[2], info[3])
pregame_advanced(advanced_averages, info[1], info[2], info[3])
