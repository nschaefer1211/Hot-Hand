#This file performs the regressions that test for the effect of heat on shot difficulty and the hot hand on a team level 
#Tables 6 and 7 are created 

library(stargazer)

#loading data. Note that this data file is not available in this repository as the data is not available for free
load("modelData.Rdata")
# renaming data for better reproducability
model <- test
model$id <- 1:dim(model)[1] #giving each observation a unique id

#Creating the Heat variable
games <- unique(test$game_id)
heat <- c()
id <- c()
for(j in 1:length(games)){
  game_j <- model %>%
    filter(game_id == games[j]) 
  teams <- unique(game_j$team)
  for(i in 1:length(teams)){
    game_j_team_i <- game_j %>%
      filter(team == teams[i])
    heat <- c(heat, rep(NA, 4))
    id <- c(id, game_j_team_i$id[1:4])
    for(k in 5:dim(game_j_team_i)[1]){
      #actual pct of made shots in the past four shots
      actual_pct <- (sum(game_j_team_i$result[(k-4):(k-1)])/4)
      #expected pct of past four shots
      exp_pct <- (sum((game_j_team_i$pred[(k-4):(k-1)]))/4)
      heat <- c(heat, (actual_pct - exp_pct))
      id <- c(id, game_j_team_i$id[k])
    }
    
  }
}

temp <- data.frame(id, heat)
temp <- temp[order(temp$id),]
model$heat <- temp$heat

############################## Regressions #############################################

#effect of heat on shot difficulty
summary(lm(pred ~ heat, data = na.omit(model)))
#latex output
stargazer(lm(pred ~ heat, data = na.omit(model)))

#testing for heat
# baseline model without controlling for shot difficulty
summary(lm(actual ~ heat + team, data= na.omit(model)))
# model that accounts for shot difficulty
summary(lm(actual ~ heat + pred, data = na.omit(model)))
#latex output
stargazer(lm(actual ~ heat + pred, data = na.omit(model)), lm(actual ~ heat + team, data= na.omit(model)))









