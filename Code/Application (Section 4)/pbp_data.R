rm(list = ls())
library(dplyr)
library(readr)
library(dplyr)
library(tidyverse)
library(stringr)


"%!in%" <- function(x, y) !("%in%"(x, y))
explore <- read.csv("data_fullseason.csv")

Viewdata <- read.csv("data_fullseason.csv") %>%
  select(game_id, date, remaining_time, play_length, period, player, event_type,
         away_score, home_score, team, type, points, result, shot_distance, description) %>%
  mutate(
    game_id = parse_number(as.character(game_id)), result = as.character(result), team = as.character(team),
    date = as.character(date), remaining_time = as.character(remaining_time), shot_distance = as.numeric(as.character(shot_distance)),
    type = as.character(type), event_type = as.character(event_type),
    description = as.character(description), remaining_time = str_sub(remaining_time, -5, -1), play_length = str_sub(play_length, -5, -1),
    remaining_time = as.numeric(substr(remaining_time, 1, 2))*60 + as.numeric(substr(remaining_time, 4,5)),
    play_length = as.numeric(substr(play_length, 1, 2))*60 + as.numeric(substr(play_length, 4,5)), home_score = as.numeric(as.character(home_score)),
    away_score = as.numeric(as.character(away_score))
  ) %>%
  filter(result %in% c("made", "missed")) %>%
  filter(
    type != "Free Throw 1 of 1", type != "Free Throw Technical", type != "Free Throw 1 of 2", type != "Free Throw 2 of 2",
    type != "Free Throw 1 of 3", type != "Free Throw 2 of 3", type != "Free Throw 3 of 3", type != "Free Throw Flagrant 1 of 1",
    type != "Free Throw Flagrant 1 of 2", type != "Free Throw Flagrant 2 of 2", type != "Free Throw Clear Path 1 of 2",
    type != "Free Throw Clear Path 2 of 2", team != "", event_type  != "free throw"
  ) %>% 
  select(-type)

#fill up the six NAs in the shot_distance data with sensible numbers (24 or 25 three-point shot, 1 for a layup)
Viewdata$shot_distance[which(is.na(Viewdata$shot_distance))] <- c(24, 24, 24, 25, 1, 24)


#for(i in 1:dim(Viewdata)[1]){
#  temp1 <- sort(c(Viewdata$a1[i], Viewdata$a2[i], Viewdata$a3[i], Viewdata$a4[i], Viewdata$a5[i]))
#  temp2 <- sort(c(Viewdata$h1[i], Viewdata$h2[i], Viewdata$h3[i], Viewdata$h4[i], Viewdata$h5[i]))
#  Viewdata$a1[i] <- temp1[1]
#  Viewdata$a2[i] <- temp1[2]
#  Viewdata$a3[i] <- temp1[3]
#  Viewdata$a4[i] <- temp1[4]
#  Viewdata$a5[i] <- temp1[5]
#  Viewdata$h1[i] <- temp2[1]
#  Viewdata$h2[i] <- temp2[2]
#  Viewdata$h3[i] <- temp2[3]
#  Viewdata$h4[i] <- temp2[4]
#  Viewdata$h5[i] <- temp2[5]
#}

#res <- sub(".*' ", "", Viewdata$description)
#res2 <- str_extract(res, "Driving Floating Bank Jump Shot|3PT Turnaround Fadeaway Bank Jump Shot|Turnaround Fadeaway Bank Jump Shot|3PT Turnaround Fadeaway Shot|Turnaround Fadeaway|Hook Bank Shot|Driving Floating Jump Shot|Step Back Bank Jump Shot|Step Back Jump Shot|Running Pull-Up Jump Bank Shot|Running Pull-Up Jump Shot|Driving Finger Roll Layup|Running Layup|Driving Layup|3PT Jump Bank Shot|3PT Pullup Jump Shot|3PT Step Back Jump Shot|3PT Jump Shot|Cutting Layup shot|Tip Layup Shot|Turnaround Bank Jump Shot|Jump Bank Shot|Cutting Dunk Shot|Pullup Jump Shot|Turnaround Hook Shot|Driving Dunk|Putback Layup|Tip Dunk Shot|Putback Dunk|Driving Finger Roll Layup|Fadeaway Jumper|Reverse Layup|Floating Jump Shot|Turnaround Jump Shot|Driving Hook Shot|Running Dunk|Alley Oop Dunk|Alley Oop Layup|Dunk|Layup|Jump Shot|Hook Shot")
#Viewdata$description <- res2

Viewdata$shot_type <- rep(NA, dim(Viewdata)[1])

for(i in 1:dim(Viewdata)[1]){
  if(!is.na(str_extract(Viewdata$description[i], "Dunk"))){
    Viewdata$shot_type[i] <- "Dunk"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Layup|Finger Roll"))){
    Viewdata$shot_type[i] <- "Layup"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "3PT"))){
    Viewdata$shot_type[i] <- "3PT Shot"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Hook Shot|Hook Bank Shot"))){
    Viewdata$shot_type[i] <- "Hook Shot"
  }
  else{
    Viewdata$shot_type[i] <- "Jump Shot"
  }
}

Viewdata$shot_diff <- rep(NA, dim(Viewdata)[1])

for(i in 1:dim(Viewdata)[1]){
  if(!is.na(str_extract(Viewdata$description[i], "Turnaround Fadeaway"))){
    Viewdata$shot_diff[i] <- "Turnaround Fadeaway"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Driving Floating"))){
    Viewdata$shot_diff[i] <- "Driving Floating"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Turnaround"))){
    Viewdata$shot_diff[i] <- "Turnaround"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Fadeaway"))){
    Viewdata$shot_diff[i] <- "Fadeaway"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Driving"))){
    Viewdata$shot_diff[i] <- "Driving"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Floating"))){
    Viewdata$shot_diff[i] <- "Floating"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Pullup"))){
    Viewdata$shot_diff[i] <- "Pullup"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Reverse"))){
    Viewdata$shot_diff[i] <- "Reverse"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Running"))){
    Viewdata$shot_diff[i] <- "Running"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Step Back"))){
    Viewdata$shot_diff[i] <- "Step Back"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Putback"))){
    Viewdata$shot_diff[i] <- "Putback"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Tip"))){
    Viewdata$shot_diff[i] <- "Tip"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Cutting"))){
    Viewdata$shot_diff[i] <- "Cutting"
  }
  else if(!is.na(str_extract(Viewdata$description[i], "Alley Oop"))){
    Viewdata$shot_diff[i] <- "Alley Oop"
  }
  else{
    Viewdata$shot_diff[i] <- "Straight Up"
  }
}

Viewdata$period <- ifelse(Viewdata$period == 5 | Viewdata$period == 6 | Viewdata$period == 7, 5, Viewdata$period)


#data <- Viewdata %>% select(-a1, -a2, -a3, -a4, -a5, -h1, -h2, -h3, -h4, -h5)
save(Viewdata, file = "pbpData.Rdata")

