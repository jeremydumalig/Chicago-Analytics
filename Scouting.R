library(tidyverse)
library(ggplot2)
library(gt)
library(gsheet)
library(janitor)
rm(list = ls())

setwd("/Users/jeremydumalig/Documents/GitHub/Chicago-Analytics")

read_csv("scout.csv") %>%
  mutate(`W/L` = case_when(`W/L` %in% c("W", "L") ~ `W/L`, 
                           TRUE ~ ""),
         `Total?` = (Opponent == "Total")) %>%
  arrange(`Total?`, desc(Margin)) %>%
  select(-`Total?`) %>%
  gt() %>%
  tab_style(style = cell_fill(color = "lightgreen"),
            locations = cells_body(rows = `W/L` == "W")) %>%
  tab_style(style = cell_fill(color = "lightcoral"),
            locations = cells_body(rows = `W/L` == "L")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(columns = everything())) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(rows = Opponent == "Total")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = Opponent)) %>%
  tab_header(title = md("Knox Advanced Statistics"),
             subtitle = "UChicago Women's Basketball Team | 12/30/22 vs Knox")

read_csv("w_case_o1.csv") %>%
  group_by(`Play Type`) %>%
  summarize(Foul = sum(Outcome == "F"),
            FGM = sum(Outcome == "1"),
            FGA = sum(Outcome == "0") + sum(Outcome == "1")) %>%
  ungroup() %>%
  mutate(Frequency = 100 * round(FGA / sum(FGA), 3),
         `Play Type` = case_when(`Play Type` == "Isolation Drive" ~ "Isolation/Drive",
                                 TRUE ~ `Play Type`)) %>%
  arrange(desc(FGA)) %>%
  adorn_totals("row") %>%
  mutate(`FG%` = case_when(FGA == 0 ~ 0,
                           TRUE ~ 100 * round(FGM / FGA, 3)),
         Frequency = case_when(`Play Type` == "Total" ~ 100,
                               TRUE ~ Frequency),
         Splits = paste(as.character(FGM), "/", as.character(FGA), sep="")) %>%
  select(`Play Type`, Frequency, Foul, Splits, `FG%`) %>%
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = `Play Type`)) %>%
  tab_header(title = md("Case: Field Goal Attempt Outcomes"),
             subtitle = "UChicago @ Case Western Reserve University | January 22, 2023")


read_csv("nyu_m_threes2.csv") %>%
  group_by(`Play Type`) %>%
  summarize(FGM = sum(Outcome == "1"),
            FGA = sum(Outcome == "0") + sum(Outcome == "1")) %>%
  ungroup() %>%
  mutate(Frequency = 100 * round(FGA / sum(FGA), 3),
         `Play Type` = case_when(`Play Type` == "Isolation Drive" ~ "Isolation/Drive",
                                 TRUE ~ `Play Type`)) %>%
  arrange(desc(FGA)) %>%
  adorn_totals("row") %>%
  mutate(`FG%` = case_when(FGA == 0 ~ 0,
                           TRUE ~ 100 * round(FGM / FGA, 3)),
         Frequency = case_when(`Play Type` == "Total" ~ 100,
                               TRUE ~ Frequency),
         Splits = paste(as.character(FGM), "/", as.character(FGA), sep="")) %>%
  select(`Play Type`, Frequency, Splits, `FG%`) %>%
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = `Play Type`)) %>%
  tab_header(title = md("NYU: 3-Point Outcomes"),
             subtitle = "UChicago @ NYU | February 10, 2023")

read_csv("mbb_tov214.csv") %>%
  group_by(Player) %>%
  summarize(`Perimeter` = sum(`Turnover Type` == "Perimeter/Strip"),
            `Pass` = sum(`Turnover Type` == "Reversal/Pass"),
            `Drive` = sum(`Turnover Type` == "Drive"),
            `Drive + Pass` = sum(`Turnover Type` == "Drive + Pass"),
            `Offensive Foul` = sum(`Turnover Type` == "Off-Ball"),
            `Post` = sum(`Turnover Type` == "Post Entry"),
            Total = n()) %>%
  ungroup() %>%
  arrange(desc(Total)) %>%
  adorn_totals("row") %>%
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(columns = everything())) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(rows = (Player == "Total"))) %>%
  tab_header(title = md("Turnover Breakdown: Player + Type"),
             subtitle = "University of Chicago Men's Basketball | Last 4 UAA Games") %>%
  tab_footnote(footnote = "Includes pick-pockets, travels, double-dribbles, out-of-bounds",
               locations = cells_column_labels(columns = `Perimeter`)) %>%
  tab_footnote(footnote = "Includes moving screens",
               locations = cells_column_labels(columns = `Offensive Foul`)) %>%
  tab_footnote(footnote = "Includes only post entry passes",
               locations = cells_column_labels(columns = `Post`))

player <- function(school, number) {
  read_csv(paste(school, number, ".csv", sep="")) %>%
    filter(!(`Play Type` %in% c("Offensive Rebound", "Transition"))) %>%
    group_by(`Play Type`) %>%
    summarize(FGM = sum(Outcome == "1"),
              FGA = sum(Outcome == "0") + sum(Outcome == "1")) %>%
    ungroup() %>%
    mutate(Frequency = 100 * round(FGA / sum(FGA), 3)) %>%
    arrange(desc(FGA)) %>%
    adorn_totals("row") %>%
    mutate(`FG%` = case_when(FGA == 0 ~ 0,
                             TRUE ~ 100 * round(FGM / FGA, 3)),
           Frequency = case_when(`Play Type` == "Total" ~ 100,
                                 TRUE ~ Frequency),
           Splits = paste(as.character(FGM), "/", as.character(FGA), sep="")) %>%
    select(`Play Type`, Frequency, Splits, `FG%`) %>%
    gt() %>%
    tab_style(style = list(cell_text(weight = "bold")),
              locations = cells_title(groups = "title")) %>%
    tab_style(style = list(cell_text(weight = "bold")),
              locations = cells_body(columns = `Play Type`))
}

player("whitman", "_all") %>%
  tab_header(title = md("Rotation Players: Shot Outcomes"),
             subtitle = "Whitman College | Last 4 Games") %>%
  tab_footnote(footnote = "Excluding offensive rebounds and transition plays",
               locations = cells_column_labels(columns = `Play Type`))
