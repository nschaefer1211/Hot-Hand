# This file creates the output of Table 9. And provides the basis for Figure 7.
# Moreover all the tests that are done with k = 4 are performed.

#loading data
setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Data") 
load("bias.Rdata") # make sure to have the right working directory
setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Code/Bias (Sections 2 and 3)")
source("GVT_replication.R") # make sure to have the right working directory
library(xtable)
library(dplyr)


ttest4 <- GVT_output %>%
  select(shooter, `P(hit|4 makes)`, `P(hit)`, `P(hit|4 misses)`, `GVT est. k = 4`) %>%
  mutate(se4 = rep(NA, dim(GVT_output)[1]))

# GVT paired t-test
t.test(GVT_final$`P(hit|4 makes)`, GVT_final$`P(hit|4 misses)`, paired = TRUE)

# calculating the pooled variance and standard error
for (i in 1:(length(GVT_output$shooter))) {
  #
  makes_after_three_makes <- GVT_table$FGM_4ma[i]
  misses_after_three_makes <- GVT_table$shots_4ma[i] - makes_after_three_makes
  sequence1 <- c(rep(1, makes_after_three_makes), rep(0, misses_after_three_makes))
  #
  makes_after_three_misses <- GVT_table$FGM_4mi[i]
  misses_after_three_misses <- GVT_table$shots_4mi[i] - makes_after_three_misses
  sequence2 <- c(rep(1, makes_after_three_misses), rep(0, misses_after_three_misses))
  # calculate pooled variance
  var1 <- var(sequence1)
  var2 <- var(sequence2)
  n1 <- length(sequence1)
  n2 <- length(sequence2)
  sdpool <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
  ttest4$se4[i] <- sdpool * sqrt((1 / n1) + (1 / n2))
}

# the exact bias for the 26 shooters calculated in data_generator.R
ttest4$bias4 <- bias$bias4

# correcting the estimate for the bias (A hat)
ttest4$adj_diff <- ttest4$`GVT est. k = 4` - ttest4$bias4
# calculating the 95 % confidence intervals for each player
ttest4$CI_lower_bound <- ttest4$adj_diff - qt(1 - 0.05 / 2, GVT_table$shots_4ma + GVT_table$shots_4mi - 2) * ttest4$se4
ttest4$CI_upper_bound <- ttest4$adj_diff + qt(1 - 0.05 / 2, GVT_table$shots_4ma + GVT_table$shots_4mi - 2) * ttest4$se4
# calculating the standard error interval for each player
ttest4$se_lower <- ttest4$adj_diff - ttest4$se4
ttest4$se_upper <- ttest4$adj_diff + ttest4$se4

# preparing table in thesis format
shooter_adj <- c(1:14, 1:12)
table4_thesis <- ttest4 %>%
  mutate(bias = bias4, `GVT est.` = `GVT est. k = 4`, `bias adj.` = adj_diff, shooter = shooter_adj) %>%
  select(shooter, `P(hit|4 makes)`, `P(hit)`, `P(hit|4 misses)`, `GVT est.`, bias, `bias adj.`)

# latex table output
xtable(table4_thesis, digits = 3)

# Miller Sanjurjo bias adjusted t-test
t.test(ttest4$adj_diff)

# normality checks
shapiro.test(GVT_final$`P(hit|4 makes)`)
shapiro.test(GVT_final$`P(hit|4 misses)`)
shapiro.test(ttest4$`GVT est. k = 4`)
shapiro.test(ttest4$adj_diff)
plot(density(na.omit(ttest4$`GVT est. k = 4`)))
plot(density(na.omit(ttest4$adj_diff)))


# mean of the bias corrected estimates that are taken into consideration
mean_adj_diff4 <- mean(ttest4$adj_diff[-c(2, 26)]) # simulation by Miller Sanjurjo: 0.1021076

ttest4_1 <- ttest4 %>%
  na.omit()

# compute the total variance
total_var4 <- sum(ttest4_1$se4^2) # simulation: 2.085446
# variance of the average difference across players
avg_var4 <- 1 / (21^2) * total_var4 # 21 eligible players for computation, simulation: 0.004728902
std_err4 <- sqrt(avg_var4) # std error across 21 players, simulation: 0.06876701
p_value4 <- 1 - pnorm(mean_adj_diff4 / std_err4) # MS 2018 round mean_adj_diff to 0.102 (mean across 24 players), simulation: 0.06879397


# compute t value with the mean of those players that were actually used to compute the std error (only 21 players) using dataframe ttest4_1
p_value4_alternative <- 1 - pnorm(mean(ttest4_1$adj_diff) / std_err4) # simulation: 0.01511654
# this gives us a far better p-value
std_err4
p_value4
p_value4_alternative


# binomial tests
# check how many players experienced significant hot-hand shooting (t-test) alpha = 0.05
eligible_shooters4 <- 0
significant_hot_hand4 <- 0
for (i in 1:26) {
  p <- pt((ttest4$adj_diff[i] / ttest4$se4[i]), df = (GVT_table$shots_4ma[i] + GVT_table$shots_4mi[i] - 2), lower.tail = F)
  if (is.na(p)) {
    next
  }
  eligible_shooters4 <- eligible_shooters4 + 1
  if (p < 0.05) {
    significant_hot_hand4 <- significant_hot_hand4 + 1
  }
}
eligible_shooters4
significant_hot_hand4

binom.test(significant_hot_hand4, eligible_shooters4, p = 0.05, "greater")
