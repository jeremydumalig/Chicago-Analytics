setwd("~/Documents/Portfolio/UC MBB")

library(tidyverse)
library(ggplot2)
library(gt)
library(gsheet)
library(janitor)
rm(list = ls())

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
