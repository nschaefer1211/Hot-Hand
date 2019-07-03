# This file generates some data used in heat.R and performs the out of sample shot difficulty test and plots Figure 4.


# loading data. Note that this data file is not available in this repository as the data is not available for free
load("pbpData.Rdata")
"%!in%" <- function(x, y) !("%in%"(x, y))
library(dplyr)



####################### creating additional variables#############################
data <- Viewdata %>%
  mutate(
    period = as.factor(period), shot_distance2 = shot_distance^2, shot_distance3 = shot_distance^3,
    score_diff = abs(home_score - away_score)
  ) %>%
  select(-description)

# shot is fored, iff it was shot during the last two seconds of a quarter
data$forced <- ifelse(data$remaining_time <= 2, 1, 0)
# fastbreak variable, shot was shot within 7 seconds of an attack
data$fastbreak <- ifelse(data$play_length <= 7, 1, 0)

games <- unique(data$game_id)
home <- c()
for (i in 1:length(games)) {
  temp <- data %>% filter(game_id == games[i])
  teams <- unique(temp$team)
  temp2 <- temp %>% filter(team == teams[1], result == "made")
  h <- 1
  for (k in 2:dim(temp2)[1]) {
    if ((temp2$home_score[k] - temp2$home_score[k - 1]) > 0) {
      next
    }
    else {
      h <- 0
    }
  }
  if (h == 1) {
    home <- c(home, ifelse(temp$team == teams[1], 1, 0))
  }
  else {
    home <- c(home, ifelse(temp$team == teams[1], 0, 1))
  }
}
# creating home dummy
data$home <- home
# switching the outcome to a binary representation
data$result <- ifelse(data$result == "made", 1, 0)


###########################################################################################################
############################################ ANALYSIS #####################################################
###########################################################################################################

# split into train and test data 50:50 split
# half the games serve as a training set
smp_size <- floor(0.5 * length(unique(data$game_id)))

## set a seed to make the partition reproducible

set.seed(176)
train_ind <- sample(unique(data$game_id), size = smp_size)

# splitting into train and test/validation set
train <- data %>%
  filter(game_id %in% train_ind) %>%
  select(-date, -away_score, -home_score, -points, -event_type, -player) %>%
  mutate(shot_type = as.factor(shot_type), shot_adddiff = as.factor(shot_adddiff), team = as.factor(team))
test <- data %>%
  filter(game_id %!in% train_ind) %>%
  select(-date, -away_score, -home_score, -points, -player) %>%
  mutate(shot_type = as.factor(shot_type), shot_adddiff = as.factor(shot_adddiff), team = as.factor(team))

# fit the regression using a regular Logit Model
fit <- glm(result ~ . - game_id, family = "binomial", data = train)
# predictions for the validation set
pred <- predict(fit, newdata = test, type = "response")

# no shot in a truly sure thing or impossible to make. Therefore, switch all ones to .99 and all zeroes to 0.01
pred <- ifelse(pred <= 0, 0.01, pred)
pred <- ifelse(pred >= 1, 0.99, pred)
# adding the predictions to the test set
test$pred <- round(pred, digits = 2)

# calculating the actual make percentages for the predicted one percent increments
actual <- rep(NA, length(unique(test$pred)))
e_prob <- sort(unique(test$pred))
for (i in 1:length(e_prob)) {
  temp1 <- test %>% filter(pred == e_prob[i], result == 1)
  temp2 <- test %>% filter(pred == e_prob[i], result == 0)
  actual[i] <- length(temp1$result) / (length(temp1$result) + length(temp2$result))
}


# plotting the fit
png(
  filename = "regshotprob.png",
  type = "cairo",
  units = "in",
  width = 6,
  height = 5,
  pointsize = 12,
  res = 250
)
plot(round(e_prob, digits = 2), round(actual, digits = 2), xlab = "Predicted P hat", main = "Out of Sample Shot Difficulty Test", ylab = "Binned Make %", pch = 16)
lines(seq(0, 1, length.out = 200), seq(0, 1, length.out = 200), col = "red", lwd = 3)
dev.off()

# adding the actual make percentage to the test set. This will be used in heat.R
test$actual <- rep(NA, dim(test)[1])
for (i in 1:dim(test)[1]) {
  for (j in 1:length(e_prob)) {
    if (test$pred[i] == e_prob[j]) {
      test$actual[i] <- actual[j]
    }
    else {
      next
    }
  }
}


save(test, file = "modelData.Rdata")
