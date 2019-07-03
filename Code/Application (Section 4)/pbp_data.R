# This file trimms and cleans the data file of the game logs of the 2017-18 NBA season

rm(list = ls())
library(dplyr)
library(readr)
library(dplyr)
library(tidyverse)
library(stringr)


# loading data. Note that this data file is not available in this repository as the data is not available for free
explore <- read.csv("data_fullseason.csv")

# defining a function that will be used throughout
"%!in%" <- function(x, y) !("%in%"(x, y))

Viewdata <- read.csv("data_fullseason.csv") %>%
  select(
    game_id, date, remaining_time, play_length, period, player, event_type,
    away_score, home_score, team, type, points, result, shot_distance, description
  ) %>%
  mutate(
    game_id = parse_number(as.character(game_id)), result = as.character(result), team = as.character(team),
    date = as.character(date), remaining_time = as.character(remaining_time), shot_distance = as.numeric(as.character(shot_distance)),
    type = as.character(type), event_type = as.character(event_type),
    description = as.character(description), remaining_time = str_sub(remaining_time, -5, -1), play_length = str_sub(play_length, -5, -1),
    remaining_time = as.numeric(substr(remaining_time, 1, 2)) * 60 + as.numeric(substr(remaining_time, 4, 5)),
    play_length = as.numeric(substr(play_length, 1, 2)) * 60 + as.numeric(substr(play_length, 4, 5)), home_score = as.numeric(as.character(home_score)),
    away_score = as.numeric(as.character(away_score))
  ) %>%
  filter(result %in% c("made", "missed")) %>%
  filter(
    type != "Free Throw 1 of 1", type != "Free Throw Technical", type != "Free Throw 1 of 2", type != "Free Throw 2 of 2",
    type != "Free Throw 1 of 3", type != "Free Throw 2 of 3", type != "Free Throw 3 of 3", type != "Free Throw Flagrant 1 of 1",
    type != "Free Throw Flagrant 1 of 2", type != "Free Throw Flagrant 2 of 2", type != "Free Throw Clear Path 1 of 2",
    type != "Free Throw Clear Path 2 of 2", team != "", event_type != "free throw"
  ) %>%
  select(-type)

# fill up the six NAs in the shot_distance data with sensible numbers (24 or 25 three-point shot, 1 for a layup)
Viewdata$shot_distance[which(is.na(Viewdata$shot_distance))] <- c(24, 24, 24, 25, 1, 24)

# creating the "shot_type"-variable
Viewdata$shot_type <- rep(NA, dim(Viewdata)[1])

for (i in 1:dim(Viewdata)[1]) {
  if (!is.na(str_extract(Viewdata$description[i], "Dunk"))) {
    Viewdata$shot_type[i] <- "Dunk"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Layup|Finger Roll"))) {
    Viewdata$shot_type[i] <- "Layup"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "3PT"))) {
    Viewdata$shot_type[i] <- "3PT Shot"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Hook Shot|Hook Bank Shot"))) {
    Viewdata$shot_type[i] <- "Hook Shot"
  }
  else {
    Viewdata$shot_type[i] <- "Jump Shot"
  }
}

# creating the "shot_adddiff" variable
Viewdata$shot_adddiff <- rep(NA, dim(Viewdata)[1])

for (i in 1:dim(Viewdata)[1]) {
  if (!is.na(str_extract(Viewdata$description[i], "Turnaround Fadeaway"))) {
    Viewdata$shot_adddiff[i] <- "Turnaround Fadeaway"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Driving Floating"))) {
    Viewdata$shot_adddiff[i] <- "Driving Floating"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Turnaround"))) {
    Viewdata$shot_adddiff[i] <- "Turnaround"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Fadeaway"))) {
    Viewdata$shot_adddiff[i] <- "Fadeaway"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Driving"))) {
    Viewdata$shot_adddiff[i] <- "Driving"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Floating"))) {
    Viewdata$shot_adddiff[i] <- "Floating"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Pullup"))) {
    Viewdata$shot_adddiff[i] <- "Pullup"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Reverse"))) {
    Viewdata$shot_adddiff[i] <- "Reverse"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Running"))) {
    Viewdata$shot_adddiff[i] <- "Running"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Step Back"))) {
    Viewdata$shot_adddiff[i] <- "Step Back"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Putback"))) {
    Viewdata$shot_adddiff[i] <- "Putback"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Tip"))) {
    Viewdata$shot_adddiff[i] <- "Tip"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Cutting"))) {
    Viewdata$shot_adddiff[i] <- "Cutting"
  }
  else if (!is.na(str_extract(Viewdata$description[i], "Alley Oop"))) {
    Viewdata$shot_adddiff[i] <- "Alley Oop"
  }
  else {
    Viewdata$shot_adddiff[i] <- "Straight Up"
  }
}

# Combining all overtime periods into one period (period 5)
Viewdata$period <- ifelse(Viewdata$period == 5 | Viewdata$period == 6 | Viewdata$period == 7, 5, Viewdata$period)


save(Viewdata, file = "pbpData.Rdata")
