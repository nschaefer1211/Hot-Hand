# This file creates the output of Table 8. And provides the basis for Figure 6.
# Moreover all the tests that are done with k = 2 are performed.

#loading data
setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Data") 
load("bias.Rdata") # make sure to have the right working directory
setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Code/Bias (Sections 2 and 3)")
source("GVT_replication.R") # make sure to have the right working directory
library(xtable)
library(dplyr)

ttest2 <- GVT_output %>%
  select(shooter, `P(hit|2 makes)`, `P(hit)`, `P(hit|2 misses)`, `GVT est. k = 2`) %>%
  mutate(se2 = rep(NA, dim(GVT_output)[1]))

# GVT paired t-test
t.test(GVT_final$`P(hit|2 makes)`, GVT_final$`P(hit|2 misses)`, paired = TRUE)

# calculating the pooled variance and standard error
for (i in 1:(length(GVT_output$shooter))) {
  #
  makes_after_three_makes <- GVT_table$FGM_2ma[i]
  misses_after_three_makes <- GVT_table$shots_2ma[i] - makes_after_three_makes
  sequence1 <- c(rep(1, makes_after_three_makes), rep(0, misses_after_three_makes))
  #
  makes_after_three_misses <- GVT_table$FGM_2mi[i]
  misses_after_three_misses <- GVT_table$shots_2mi[i] - makes_after_three_misses
  sequence2 <- c(rep(1, makes_after_three_misses), rep(0, misses_after_three_misses))
  # calculate pooled variance
  var1 <- var(sequence1)
  var2 <- var(sequence2)
  n1 <- length(sequence1)
  n2 <- length(sequence2)
  sdpool <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
  ttest2$se2[i] <- sdpool * sqrt((1 / n1) + (1 / n2))
  ttest2$var[i] <- (ttest2$se2[i])^2
}

# the exact bias for the 26 shooters calculated in data_generator.R
ttest2$bias2 <- bias$bias2

# correcting the estimate for the bias (A hat)
ttest2$adj_diff <- ttest2$`GVT est. k = 2` - ttest2$bias2
# calculating the 95 % confidence intervals for each player
ttest2$CI_lower_bound <- ttest2$adj_diff - qt(1 - 0.05 / 2, GVT_table$shots_2ma + GVT_table$shots_2mi - 2) * ttest2$se2
ttest2$CI_upper_bound <- ttest2$adj_diff + qt(1 - 0.05 / 2, GVT_table$shots_2ma + GVT_table$shots_2mi - 2) * ttest2$se2
# calculating the standard error interval for each player
ttest2$se_lower <- ttest2$adj_diff - ttest2$se2
ttest2$se_upper <- ttest2$adj_diff + ttest2$se2

# preparing table in thesis format
shooter_adj <- c(1:14, 1:12)
table2_thesis <- ttest2 %>%
  mutate(bias = bias2, `GVT est.` = `GVT est. k = 2`, `bias adj.` = adj_diff, shooter = shooter_adj) %>%
  select(shooter, `P(hit|2 makes)`, `P(hit)`, `P(hit|2 misses)`, `GVT est.`, bias, `bias adj.`)

# latex table output
xtable(table2_thesis, digits = 3)

# Miller Sanjurjo bias adjusted t-test
t.test(ttest2$adj_diff)


# normality checks
shapiro.test(GVT_final$`P(hit|2 makes)`)
shapiro.test(GVT_final$`P(hit|2 misses)`)
shapiro.test(ttest2$`GVT est. k = 2`)
shapiro.test(ttest2$adj_diff)
plot(density(ttest2$`GVT est. k = 2`))
plot(density(ttest2$adj_diff))

# mean of the bias corrected estimates
mean_adj_diff2 <- mean(ttest2$adj_diff)

# compute the total variance
total_var2 <- sum(ttest2$se2^2) # simulation: 0.6336564
# variance of the average difference across players
avg_var2 <- 1 / (26^2) * total_var2 # simulation: 0.0009373616
std_err2 <- sqrt(avg_var2) # simulation: 0.03061636
p_value2 <- 1 - pnorm(mean_adj_diff2 / std_err2) # simulation: 0.03958073
std_err2
p_value2


# binomial tests
# check how many players experienced significant hot-hand shooting (t-test) alpha = 0.05
eligible_shooters2 <- 0
significant_hot_hand2 <- 0
for (i in 1:26) {
  p <- pt((ttest2$adj_diff[i] / ttest2$se2[i]), df = (GVT_table$shots_2ma[i] + GVT_table$shots_2mi[i] - 2), lower.tail = F)
  if (!is.na(p)) {
    eligible_shooters2 <- eligible_shooters2 + 1
  }
  if (p < 0.05) {
    significant_hot_hand2 <- significant_hot_hand2 + 1
  }
}
eligible_shooters2
significant_hot_hand2

binom.test(significant_hot_hand2, eligible_shooters2, p = 0.05, "greater")
