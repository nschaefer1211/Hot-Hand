##############

load("pbpData.Rdata")

#creating additional variables
data <- Viewdata %>%
  mutate(period = as.factor(period), shot_distance2 = shot_distance^2, shot_distance3 = shot_distance^3,
         score_diff = abs(home_score - away_score)) %>%
  select(-description)


data$forced <- ifelse(data$remaining_time <= 2, 1, 0)
data$fastbreak <- ifelse(data$play_length <= 7, 1, 0)

games <- unique(data$game_id)
home <- c()
for(i in 1:length(games)){
  temp <- data %>% filter(game_id == games[i])
  teams <- unique(temp$team)
  temp2 <- temp %>% filter(team == teams[1], result == "made")
  h <- 1
  for(k in 2:dim(temp2)[1]){
    if((temp2$home_score[k] - temp2$home_score[k-1]) > 0){
      next
    }
    else{
      h <- 0
    }
  }
  if(h == 1){
    home <- c(home, ifelse(temp$team == teams[1], 1, 0))
  }
  else{
    home <- c(home, ifelse(temp$team == teams[1], 0, 1))
  }
}
data$home <- home
#data$lineup <- as.factor(ifelse(data$home == 1, paste(data$h1, data$h2, data$h3, data$h4, data$h5), 
                              #  paste(data$a1, data$a2, data$a3, data$a4, data$a5)))
data$result <- ifelse(data$result == "made", 1, 0)


###########################################################################################################
############################################ ANALYSIS #####################################################
###########################################################################################################

#split into train and test data
smp_size <- floor(0.5* nrow(data))

## set the seed to make your partition reproducible
set.seed(122)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ] %>%
  select(-game_id, -date, -away_score, -home_score, -points, -event_type, -player) %>%
  mutate(shot_type = as.factor(shot_type), shot_diff = as.factor(shot_diff), team = as.factor(team))
test <- data[-train_ind, ] %>%
  select(-game_id, -date, -away_score, -home_score, -points, -player) %>%
  mutate(shot_type = as.factor(shot_type), shot_diff = as.factor(shot_diff), team = as.factor(team))

#null<- glm(result ~ 1, family = binomial (link = "cloglog"), data = train)
#full <- glm(result ~., family = binomial (link = "cloglog"), data = train)

#forward <-  step(null, scope=list(lower=null, upper=full), direction="forward")
#backward <- step(full, data=train, direction="backward")
#both <- step(null, scope = list(upper=full), data=train, direction="both")


fit <- glm(result ~ ., family = binomial, data = train)
pred <- round(predict(fit, newdata = test, type = "response"), digits = 2)

pred <- ifelse(pred <= 0, 0.01, pred)
pred <- ifelse(pred >= 1, 0.99, pred)
test$pred <- pred
actual <- rep(NA, length(unique(test$pred)))
e_prob <- sort(unique(test$pred))
for(i in 1:length(e_prob)){
  temp1 <- test %>% filter(pred == e_prob[i], result == 1)
  temp2 <- test %>% filter(pred == e_prob[i], result == 0)
  actual[i] <- length(temp1$result)/(length(temp1$result) + length(temp2$result))
}

sum(abs(actual - e_prob))
plot(actual, e_prob)
lines(seq(0, 1, length.out = 200), seq(0, 1, length.out = 200), col = "red")
