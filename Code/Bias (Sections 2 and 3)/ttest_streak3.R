# This file creates the output of Table 4. And provides the data generation for Figure 3.
# Moreover all the tests that are done with k = 3 are performed.

#loading data
setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Data") 
load("bias.Rdata") # make sure to have the right working directory
setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Code/Bias (Sections 2 and 3)")
source("GVT_replication.R") # make sure to have the right working directory
library(xtable)
library(dplyr)


ttest3 <- GVT_output %>%
  select(shooter, `P(hit|3 makes)`, `P(hit)`, `P(hit|3 misses)`, `GVT est. k = 3`) %>%
  mutate(se3 = rep(NA, dim(GVT_output)[1]))

# GVT paired t-test
t.test(GVT_final$`P(hit|3 makes)`, GVT_final$`P(hit|3 misses)`, paired = TRUE)

# calculating the pooled variance and standard error
for (i in 1:(length(GVT_output$shooter))) {
  #
  makes_after_three_makes <- GVT_table$FGM_3ma[i]
  misses_after_three_makes <- GVT_table$shots_3ma[i] - makes_after_three_makes
  sequence1 <- c(rep(1, makes_after_three_makes), rep(0, misses_after_three_makes))
  #
  makes_after_three_misses <- GVT_table$FGM_3mi[i]
  misses_after_three_misses <- GVT_table$shots_3mi[i] - makes_after_three_misses
  sequence2 <- c(rep(1, makes_after_three_misses), rep(0, misses_after_three_misses))
  # calculate pooled variance
  var1 <- var(sequence1)
  var2 <- var(sequence2)
  n1 <- length(sequence1)
  n2 <- length(sequence2)
  sdpool <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
  ttest3$se3[i] <- sdpool * sqrt((1 / n1) + (1 / n2))
}

# the exact bias for the 26 shooters calculated in data_generator.R
ttest3$bias3 <- bias$bias3

# correcting the estimate for the bias (A hat)
ttest3$adj_diff <- ttest3$`GVT est. k = 3` - ttest3$bias3
# calculating the 95 % confidence intervals for each player
ttest3$CI_lower_bound <- ttest3$adj_diff - qt(1 - 0.05 / 2, GVT_table$shots_3ma + GVT_table$shots_3mi - 2) * ttest3$se3
ttest3$CI_upper_bound <- ttest3$adj_diff + qt(1 - 0.05 / 2, GVT_table$shots_3ma + GVT_table$shots_3mi - 2) * ttest3$se3
# calculating the standard error interval for each player
ttest3$se_lower <- ttest3$adj_diff - ttest3$se3
ttest3$se_upper <- ttest3$adj_diff + ttest3$se3

# preparing table in thesis format
shooter_adj <- c(1:14, 1:12)
table3_thesis <- ttest3 %>%
  mutate(bias = bias3, `GVT est.` = `GVT est. k = 3`, `bias adj.` = adj_diff, shooter = shooter_adj) %>%
  select(shooter, `P(hit|3 makes)`, `P(hit)`, `P(hit|3 misses)`, `GVT est.`, bias, `bias adj.`)

# latex table output
xtable(table3_thesis, digits = 3)

# Miller Sanjurjo bias adjusted t-test
t.test(ttest3$adj_diff)


# normality checks
shapiro.test(GVT_final$`P(hit|3 makes)`)
shapiro.test(GVT_final$`P(hit|3 misses)`)
shapiro.test(ttest3$`GVT est. k = 3`)
shapiro.test(ttest3$adj_diff)
plot(density(na.omit(ttest3$`GVT est. k = 3`)))
plot(density(na.omit(ttest3$adj_diff)))

# mean of the bias corrected estimates that are taken into consideration
mean_adj_diff3 <- mean(ttest3$adj_diff[-26])
# data frame that omits the NAs
ttest3_1 <- ttest3 %>%
  na.omit()

# compute the total variance
total_var3 <- sum(ttest3_1$se3^2) # simulation: 1.357972
# variance of the average difference across players
avg_var3 <- 1 / (25^2) * total_var3 # 25 eligible player for computation, simulation: 0.002172755
std_err3 <- sqrt(avg_var3) # simulation: 0.04661282
p_value3 <- 1 - pnorm(mean_adj_diff3 / std_err3) # simulation: 0.003658314
std_err3
p_value3


# binomial tests
# check how many players experienced significant hot-hand shooting (t-test) alpha = 0.05
eligible_shooters3 <- 0
significant_hot_hand3 <- 0
for (i in 1:26) {
  p <- pt((ttest3$adj_diff[i] / ttest3$se3[i]), df = (GVT_table$shots_3ma[i] + GVT_table$shots_3mi[i] - 2), lower.tail = F)
  if (is.na(p)) {
    next
  }
  eligible_shooters3 <- eligible_shooters3 + 1
  if (p < 0.05) {
    significant_hot_hand3 <- significant_hot_hand3 + 1
  }
}
eligible_shooters3
significant_hot_hand3

binom.test(significant_hot_hand3, eligible_shooters3, p = 0.05, "greater")
