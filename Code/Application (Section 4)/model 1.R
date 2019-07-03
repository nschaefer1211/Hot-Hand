# This script performs the GVT-type analysis on the NBA data set and produces the output for Table 5

library(xtable)
library(dplyr)

# Model 1
# loading data. Note that this data file is not available in this repository as the data is not available for free
load("pbpData.Rdata")
# renaming data for better reproducability
data <- Viewdata

teams <- unique(data$team)


# creating the needed variables and store them in a data frame
table <- data.frame(
  Team = c("All"), shots = c(length(data$result)), FGM = length((data %>% filter(result == "made"))$result),
  shot_distance = c(mean(data$shot_distance)), shots_3ma = c(NA), FGM_3ma = c(NA), dist_3ma = c(NA),
  shots_3mi = c(NA), FGM_3mi = c(NA), dist_3mi = c(NA), shots_4ma = c(NA), FGM_4ma = c(NA), dist_4ma = c(NA),
  shots_4mi = c(NA), FGM_4mi = c(NA), dist_4mi = c(NA)
)

for (j in 1:length(teams)) {
  team_j <- data %>%
    filter(team == teams[j])
  games <- unique(team_j$game_id)
  temp1 <- as.data.frame(matrix(ncol = length(names(data)), nrow = 10000))
  colnames(temp1) <- colnames(data)
  counter1 <- 0
  temp2 <- as.data.frame(matrix(ncol = length(names(data)), nrow = 10000))
  colnames(temp2) <- colnames(data)
  counter2 <- 0
  temp3 <- as.data.frame(matrix(ncol = length(names(data)), nrow = 10000))
  colnames(temp3) <- colnames(data)
  counter3 <- 0
  temp4 <- as.data.frame(matrix(ncol = length(names(data)), nrow = 10000))
  colnames(temp4) <- colnames(data)
  counter4 <- 0
  for (i in 1:length(games)) {
    team_j_game_i <- team_j %>%
      filter(game_id == games[i])

    counter <- 0
    for (i in 1:length(team_j_game_i$result)) {
      if (counter >= 3) {
        counter1 <- counter1 + 1
        temp1[counter1, ] <- team_j_game_i[i, ]
      }
      if (team_j_game_i$result[i] == "made") {
        counter <- counter + 1
      }
      else {
        counter <- 0
      }
    }

    counter <- 0
    for (i in 1:length(team_j_game_i$result)) {
      if (counter >= 3) {
        counter2 <- counter2 + 1
        temp2[counter2, ] <- team_j_game_i[i, ]
      }
      if (team_j_game_i$result[i] == "missed") {
        counter <- counter + 1
      }
      else {
        counter <- 0
      }
    }

    counter <- 0
    for (i in 1:length(team_j_game_i$result)) {
      if (counter >= 4) {
        counter3 <- counter3 + 1
        temp3[counter3, ] <- team_j_game_i[i, ]
      }
      if (team_j_game_i$result[i] == "made") {
        counter <- counter + 1
      }
      else {
        counter <- 0
      }
    }

    counter <- 0
    for (i in 1:length(team_j_game_i$result)) {
      if (counter >= 4) {
        counter4 <- counter4 + 1
        temp4[counter4, ] <- team_j_game_i[i, ]
      }
      if (team_j_game_i$result[i] == "missed") {
        counter <- counter + 1
      }
      else {
        counter <- 0
      }
    }
  }
  temp1 <- temp1 %>% na.omit()
  temp2 <- temp2 %>% na.omit()
  temp3 <- temp3 %>% na.omit()
  temp4 <- temp4 %>% na.omit()

  temp <- data.frame(
    Team = c(teams[j]),
    shots = c(length(team_j$result)), FGM = c(length((team_j %>% filter(result == "made"))$result)), shot_distance = c(mean(team_j$shot_distance)),
    shots_3ma = c(length(temp1$result)), FGM_3ma = c(length((temp1 %>% filter(result == "made"))$result)), dist_3ma = c(mean(temp1$shot_distance)),
    shots_3mi = c(length(temp2$result)), FGM_3mi = c(length((temp2 %>% filter(result == "made"))$result)), dist_3mi = c(mean(temp2$shot_distance)),
    shots_4ma = c(length(temp3$result)), FGM_4ma = c(length((temp3 %>% filter(result == "made"))$result)), dist_4ma = c(mean(temp3$shot_distance)),
    shots_4mi = c(length(temp4$result)), FGM_4mi = c(length((temp4 %>% filter(result == "made"))$result)), dist_4mi = c(mean(temp4$shot_distance))
  )

  table <- rbind(temp, table)
}

table[31, "shots_3ma"] <- sum(table[-31, "shots_3ma"])
table[31, "FGM_3ma"] <- sum(table[-31, "FGM_3ma"])
table[31, "dist_3ma"] <- sum(table[-31, "dist_3ma"] * table[-31, "shots_3ma"]) / table[31, "shots_3ma"]

table[31, "shots_3mi"] <- sum(table[-31, "shots_3mi"])
table[31, "FGM_3mi"] <- sum(table[-31, "FGM_3mi"])
table[31, "dist_3mi"] <- sum(table[-31, "dist_3mi"] * table[-31, "shots_3mi"]) / table[31, "shots_3mi"]

table[31, "shots_4ma"] <- sum(table[-31, "shots_4ma"])
table[31, "FGM_4ma"] <- sum(table[-31, "FGM_4ma"])
table[31, "dist_4ma"] <- sum(table[-31, "dist_4ma"] * table[-31, "shots_4ma"]) / table[31, "shots_4ma"]

table[31, "shots_4mi"] <- sum(table[-31, "shots_4mi"])
table[31, "FGM_4mi"] <- sum(table[-31, "FGM_4mi"])
table[31, "dist_4mi"] <- sum(table[-31, "dist_4mi"] * table[-31, "shots_4mi"]) / table[31, "shots_4mi"]

# adjusting the data frame to create the conditional probabilitites
table_final <- table %>%
  mutate(
    `FG%` = FGM / shots, `P(hit|3 makes)` = FGM_3ma / shots_3ma, `P(hit|3 misses)` = FGM_3mi / shots_3mi, `P(hit|4 makes)` = FGM_4ma / shots_4ma,
    `P(hit|4 misses)` = FGM_4mi / shots_4mi
  ) %>%
  select(
    Team, shots, `FG%`, shot_distance, shots_3ma, `P(hit|3 makes)`, dist_3ma, shots_3mi, `P(hit|3 misses)`, dist_3mi,
    shots_4ma, `P(hit|4 makes)`, dist_4ma, shots_4mi, `P(hit|4 misses)`, dist_4mi
  )

# putting data frame into output format (putting shots in brackets etc.) and creating the estimators
output <- table_final[-31, ] %>%
  select(Team, `P(hit|4 misses)`, shots_4mi, `P(hit|3 misses)`, shots_3mi, `FG%`, shots, `P(hit|3 makes)`, shots_3ma, `P(hit|4 makes)`, shots_4ma) %>%
  mutate(
    Team = as.character(Team),
    `GVT est. k = 3` = round(`P(hit|3 makes)` - `P(hit|3 misses)`, digits = 3),
    `GVT est. k = 4` = round(`P(hit|4 makes)` - `P(hit|4 misses)`, digits = 3),
    `P(hit|4 misses)` = paste(round(`P(hit|4 misses)`, digits = 3), paste("(", shots_4mi, ")", sep = "")),
    `P(hit|3 misses)` = paste(round(`P(hit|3 misses)`, digits = 3), paste("(", shots_3mi, ")", sep = "")),
    `P(hit)` = paste(round(`FG%`, digits = 3), paste("(", shots, ")", sep = "")),
    `P(hit|3 makes)` = paste(round(`P(hit|3 makes)`, digits = 3), paste("(", shots_3ma, ")", sep = "")),
    `P(hit|4 makes)` = paste(round(`P(hit|4 makes)`, digits = 3), paste("(", shots_4ma, ")", sep = ""))
  ) %>%
  select(-shots_4mi, -shots_3mi, -shots, -shots_3ma, -shots_4ma, -`FG%`) %>%
  select(Team, `P(hit|4 misses)`, `P(hit|3 misses)`, `P(hit)`, `P(hit|3 makes)`, `P(hit|4 makes)`, `GVT est. k = 3`, `GVT est. k = 4`)

# creating the last row (average row)
avg <- c(
  "Average", paste(round(mean(table_final$`P(hit|4 misses)`), digits = 3), paste("(", table_final$shots_4mi[31], ")", sep = "")),
  paste(round(mean(table_final$`P(hit|3 misses)`), digits = 3), paste("(", table_final$shots_3mi[31], ")", sep = "")),
  paste(round(mean(table_final$`FG%`), digits = 3), paste("(", table_final$shots[31], ")", sep = "")),
  paste(round(mean(table_final$`P(hit|3 makes)`), digits = 3), paste("(", table_final$shots_3ma[31], ")", sep = "")),
  paste(round(mean(table_final$`P(hit|4 makes)`), digits = 3), paste("(", table_final$shots_4ma[31], ")", sep = "")),
  mean(output$`GVT est. k = 3`), mean(output$`GVT est. k = 4`)
)

# combining the rows
output <- rbind(output, avg)

# latex output
xtable(output, digits = 3)
