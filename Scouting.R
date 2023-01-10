library(tidyverse)
library(ggplot2)
library(gt)
library(gsheet)
library(janitor)
rm(list = ls())

setwd("/Users/jeremydumalig/Documents/GitHub/Chicago-Analytics")

scout <- 
  read_csv("scout.csv") %>%
  mutate(`W/L` = case_when(`W/L` %in% c("W", "L") ~ `W/L`, 
                           TRUE ~ ""),
         `Total?` = (Opponent == "Total")) %>%
  arrange(`Total?`, desc(Margin)) %>%
  select(-`Total?`) 

scout %>%
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

read_csv("grudzinski.csv") %>%
  group_by(`Shot Type`) %>%
  summarize(FGM = sum(Outcome),
            FGA = n(),
            `FG%` = 100 * round(FGM / FGA, 3)) %>%
  ungroup() %>%
  mutate(Frequency = 100 * round(FGA / sum(FGA), 3),
         Splits = paste(as.character(FGM), "/", as.character(FGA), sep="")) %>%
  arrange(desc(FGA)) %>%
  select(`Shot Type`, Frequency, Splits, `FG%`) %>%
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = `Shot Type`)) %>%
  tab_header(title = md("Will Grudzinski"),
             subtitle = "Washington University | Last 5 Games")

read_csv("nyu_drives.csv") %>%
  group_by(`Play Type`) %>%
  summarize(Count = n(),
            Turnover = sum(Outcome == "T"),
            Foul = sum(Outcome == "F"),
            FGM = sum(Outcome == "1"),
            FGA = sum(Outcome == "0") + sum(Outcome == "1"),
            Pass = sum(Outcome == "P")) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  adorn_totals("row") %>%
  mutate(`FG%` = 100 * round(FGM / FGA, 3)) %>%
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = `Play Type`)) %>%
  tab_header(title = md("Drive Outcomes"),
             subtitle = "New York University | 3 Closest Games")

read_csv("nyu_pnr.csv") %>%
  group_by(`Play Type`) %>%
  summarize(Count = n(),
            Turnover = sum(Outcome == "T"),
            Foul = sum(Outcome == "F"),
            FGM = sum(Outcome == "1"),
            FGA = sum(Outcome == "0") + sum(Outcome == "1"),
            Pass = sum(Outcome == "P")) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  adorn_totals("row") %>%
  mutate(`FG%` = 100 * round(FGM / FGA, 3)) %>%
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = `Play Type`)) %>%
  tab_header(title = md("Pick-and-Roll Outcomes"),
             subtitle = "New York University | 3 Closest Games")
