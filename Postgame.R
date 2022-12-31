library(tidyverse)
library(ggplot2)
library(gsheet)
library(gt)
rm(list = ls())

setwd("/Users/jeremydumalig/Documents/GitHub/Chicago-Analytics")

raw_head_to_head <- read_csv(file = "head_to_head.csv")
traditional <- read_csv(file = "traditional_comparisons.csv")
advanced <- read_csv(file = "advanced_comparison.csv")

info <- c('Lutheran', 'Away', 'December 18, 2022')

####################

head_to_head <-
  select(raw_head_to_head,
         PTS, FG, `FG%`, `3PT`, `3P%`, FT, `FT%`,
         AST, REB, OREB, DREB, STL, BLK, PF,
         TOV,
         `PTS OFF TOV`,
         PAINT,
         FASTBREAK,
         BENCH,
         `2ND CHANCE`)

traditional <- 
  select(traditional,
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
  if (substr(format(number, nsmall = 1), 1, 1) != "-") {
    return(paste("+", format(number, nsmall = 1), sep = ""))
  } else {
    return(paste("-", format(abs(number), nsmall = 1), sep = ""))
  }
}

postgame_traditional <-
  function(dataframe, type, opponent, date) {
    dataframe %>%
      t() %>%
      data.frame() %>%
      select(X3, X1, X2) %>%
      gt() %>%
      cols_label(X3 = '',
                 X1 = raw_head_to_head$Team[1],
                 X2 = raw_head_to_head$Team[2]) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = X3)) %>%
      tab_header(title = md(paste('**Post-Game Report: ', type, '**', sep =
                                    '')),
                 subtitle = date)
  }
traditional_comparison <-
  function(dataframe, type, opponent, date) {
    dataframe <-
      dataframe %>%
      t() %>%
      data.frame() %>%
      select(X3, X1, X4, X2, X5) %>%
      mutate(Diff1 = as.numeric(X4) - as.numeric(X1),
             Diff2 = as.numeric(X5) - as.numeric(X2))
    dataframe %>%
      mutate(
        Diff1 = map(dataframe$Diff1, differential_sign),
        Diff2 = map(dataframe$Diff2, differential_sign)
      ) %>%
      gt() %>%
      cols_label(
        X3 = '',
        X1 = 'Average',
        Diff1 = '',
        X2 = 'Average',
        X4 = 'Game',
        Diff2 = '',
        X5 = 'Game'
      ) %>%
      tab_spanner(label = "UChicago",
                  columns = c(X1, Diff1, X4))  %>%
      tab_spanner(label = opponent,
                  columns = c(X2, Diff2, X5)) %>%
      tab_style(style = list(cell_text(style = "italic", size = "small")),
                locations = cells_body(columns = c(Diff1, Diff2))) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = X3)) %>%
      tab_header(title = md(paste(
        '**Post-Game Report: ', type, ' Comparison**', sep = ''
      )),
      subtitle = date)
  }
advanced_comparison <-
  function(dataframe, opponent, date) {
    dataframe <-
      dataframe %>%
      select(-`...1`,-Team) %>%
      t() %>%
      data.frame() %>%
      select(X3, X1, X4, X2, X5) %>%
      mutate(Diff1 = as.numeric(X4) - as.numeric(X1),
             Diff2 = as.numeric(X5) - as.numeric(X2))
    dataframe %>%
      mutate(
        Diff1 = map(dataframe$Diff1, differential_sign),
        Diff2 = map(dataframe$Diff2, differential_sign)
      ) %>%
      gt() %>%
      cols_label(
        X3 = '',
        X1 = 'Average',
        Diff1 = '',
        X2 = 'Average',
        X4 = 'Game',
        Diff2 = '',
        X5 = 'Game'
      ) %>%
      tab_spanner(label = "UChicago",
                  columns = c(X1, Diff1, X4))  %>%
      tab_spanner(label = opponent,
                  columns = c(X2, Diff2, X5)) %>%
      tab_style(style = list(cell_text(style = "italic", size = "small")),
                locations = cells_body(columns = c(Diff1, Diff2))) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = X3)) %>%
      tab_header(
        title = md('**Post-Game Report: Advanced Comparison**'),
        subtitle = date
      )
  }

postgame_traditional(head_to_head, 'Head-to-Head', info[1], info[3])
traditional_comparison(traditional, 'Traditional', info[1], info[3])
advanced_comparison(advanced, info[1], info[3])
