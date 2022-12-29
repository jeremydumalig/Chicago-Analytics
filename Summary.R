library(tidyverse)
library(gt)
library(gsheet)
library(janitor)
rm(list = ls())

mbb_url <- "https://docs.google.com/spreadsheets/d/1BcIP7CIYDTNnedcRG3U3HAIwluaJtz8LCAKfArlEh98/edit#gid=2032963821"
wbb_url <- "https://docs.google.com/spreadsheets/d/12JWqAMfVrSZobLohmxQK6PyrbNbOQbM6ux4LxXqWenM/edit#gid=2032963821"

mens_team <- FALSE
date <- "12/18/22"
opponent <- "Lutheran"

if (mens_team) {
  url <- mbb_url
  raw_posts <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1BcIP7CIYDTNnedcRG3U3HAIwluaJtz8LCAKfArlEh98/edit#gid=1904120862")
  raw_turnovers <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1BcIP7CIYDTNnedcRG3U3HAIwluaJtz8LCAKfArlEh98/edit#gid=2056191088")
} else {
  url <- wbb_url
  raw_posts <- gsheet2tbl("https://docs.google.com/spreadsheets/d/12JWqAMfVrSZobLohmxQK6PyrbNbOQbM6ux4LxXqWenM/edit#gid=748348799")
  raw_turnovers <- gsheet2tbl("https://docs.google.com/spreadsheets/d/12JWqAMfVrSZobLohmxQK6PyrbNbOQbM6ux4LxXqWenM/edit#gid=2056191088")
}

raw_shots <- gsheet2tbl(url) %>%
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
  ))

raw_shots <- filter(raw_shots, Date == date)
raw_posts <- filter(raw_posts, Date == date)
raw_turnovers <- filter(raw_turnovers, Date == date)

raw_turnovers %>%
  filter(Player != opponent) %>%
  group_by(`Turnover Type`) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  gt()

raw_turnovers %>%
  filter(Player != opponent) %>%
  group_by(Player) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  gt()

raw_posts %>%
  mutate(Outcome = case_when(
    Outcome == 0 ~ "Miss",
    Outcome == 1 ~ "Make",
    TRUE ~ Outcome
  )) %>%
  filter(Player != opponent) %>%
  group_by(Outcome) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  gt()


mbb_game_dates <- c("11/10/22", "11/12/22", "11/13/22", "11/18/22", "11/19/22", "11/26/22", "11/30/22")
players <- c('Alec Bryan', 'Kerem Ozturk', 'Leonardo Edo', 'Skyler Twyman', 'Ezra Moos', 'Thomas Kurowski', 'Dashiel Walker', 'Elliot Paschal', 'Arrish Bhandal', 'Josh Preston', 'Jackson Piotrowski', 'Bryce Hopkins', 'Tola Olorode', 'Joe Berry', 'Ben Chasin', 'Blake Hawthorne', 'Eamonn Kenah', 'Alex Battist')

raw_turnovers %>%
  filter(!Date %in% mbb_game_dates,
         Player %in% players) %>%
  mutate(`Post Entry` = (`Turnover Type` == "Post Entry"),
         `Perimeter/Strip` = (`Turnover Type` == "Perimeter/Strip"),
         `Drive` = (`Turnover Type` == "Drive"),
         `Drive + Pass` = (`Turnover Type` == "Drive + Pass"),
         `Reversal/Pass` = (`Turnover Type` == "Reversal/Pass"),
         `Off-Ball` = (`Turnover Type` == "Off-Ball")) %>%
  group_by(Player) %>%
  summarize(`Post Entry` = sum(`Post Entry`),
            `Perimeter/Strip` = sum(`Perimeter/Strip`),
            `Drive` = sum(`Drive`),
            `Drive + Pass` = sum(`Drive + Pass`),
            `Reversal/Pass` = sum(`Reversal/Pass`),
            `Off-Ball` = sum(`Off-Ball`),
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
            locations = cells_body(columns = Total)) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(rows = Player == "Total")) %>%
  tab_header(title = md("Turnover Type by Player"),
             subtitle = "2022 Preseason | UChicago Men's Basketball Team") %>%
  tab_footnote(footnote = "Includes travels, double-dribbles, out-of-bounds (unless occuring on a drive)",
               locations = cells_column_labels(columns = `Perimeter/Strip`)) %>%
  tab_footnote(footnote = "Includes illegal screens, three-in-the-key, away-from-the-ball offensive fouls",
               locations = cells_column_labels(columns = `Off-Ball`))

raw_shots %>%
  filter(Player %in% teams) %>%
  group_by(Region) %>%
  summarize(FGM = sum(Outcome),
            FGA = n()) %>%
  ungroup() %>%
  mutate(`FG%` = 100 * round(FGM/FGA, 3)) %>%
  arrange(desc(`FG%`)) %>%
  gt()
