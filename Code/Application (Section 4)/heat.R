# Actual % over past n shots - Expected % over past n shots
library(stargazer)


load("modelData.Rdata")

model <- test
model$id <- 1:dim(model)[1]

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

summary(lm(actual ~ heat + pred, data = na.omit(model)))
summary(lm(actual ~ heat + team, data= na.omit(model)))
#latex output
stargazer(lm(actual ~ heat + pred, data = na.omit(model)), lm(actual ~ heat + team, data= na.omit(model)))
#shot difficulty
summary(lm(pred ~ heat, data = na.omit(model)))
#latex output
stargazer(lm(pred ~ heat, data = na.omit(model)))









# apply the found model on all the data
red <- data %>%
  select(-game_id, -date, -away_score, -home_score, -points, -event_type, -player) %>%
  mutate(shot_type = as.factor(shot_type), shot_adddiff = as.factor(shot_adddiff), team = as.factor(team))

OLS <- glm(result ~., family = binomial, data = red)
shot_pred <- round(predict(fit, newdata = data, type = "response", digits = 2))
shot_pred <- ifelse(shot_pred <= 0, 0.01, shot_pred)
shot_pred <- ifelse(shot_pred >= 1, 0.99, shot_pred)
data$shot_pred <- shot_pred


data$actual <- rep(NA, length(unique(data$shot_pred)))
e_prob <- sort(unique(test$pred))
for(i in 1:length(e_prob)){
  temp1 <- test %>% filter(pred == e_prob[i], result == 1)
  temp2 <- test %>% filter(pred == e_prob[i], result == 0)
  actual[i] <- length(temp1$result)/(length(temp1$result) + length(temp2$result))
}


