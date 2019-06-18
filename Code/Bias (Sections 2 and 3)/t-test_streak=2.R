setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Code/Bias (Sections 2 and 3)")
source("GVT_replication.R")
load("bias.Rdata")
library(xtable)

ttest2 <- GVT_output %>%
  select(shooter, `P(hit|2 makes)`, `P(hit)`, `P(hit|2 misses)`, `GVT est. k = 2`) %>%
  mutate(se2 = rep(NA, dim(GVT_output)[1]))

for(i in 1:(length(GVT_output$shooter))){
  #
  makes_after_three_makes <- GVT_table$FGM_2ma[i]
  misses_after_three_makes <- GVT_table$shots_2ma[i] - makes_after_three_makes
  sequence1 <- c(rep(1, makes_after_three_makes), rep(0, misses_after_three_makes))
  #
  makes_after_three_misses <- GVT_table$FGM_2mi[i]
  misses_after_three_misses <- GVT_table$shots_2mi[i] - makes_after_three_misses
  sequence2 <- c(rep(1, makes_after_three_misses), rep(0, misses_after_three_misses))
  #calculate pooled variance
  var1 <- var(sequence1)
  var2 <- var(sequence2)
  n1 <- length(sequence1)
  n2 <- length(sequence2)
  sdpool <- sqrt(((n1-1) * var1 + (n2 -1) * var2)/(n1+n2-2))
  ttest2$se2[i] <- sdpool * sqrt((1/n1) + (1/n2))
  ttest2$var[i] <- (ttest2$se2[i])^2
}
# import from Miller and Sanjurjo: bias from simulations
#ttest2$bias2 <- c(
#  -.0322355, -.0357186, -.0338025, -.0370341, -.0335411, -.0324078, -.0435676, -.0644038, -.0319288, -.0342103, -.0339279,
#  -.0326582, -.0341199, -.0328091, -.0335334, -.0353176, -.0338692, -.0379152, -.0358452, -.0313152, -.0337457, -.0311797,
#  -.0325102, -.0320413, -.0319271, -.0460187
#)

#exact bias
ttest2$bias2 <- bias$bias2


ttest2$adj_diff <- ttest2$`GVT est. k = 2` - ttest2$bias2
ttest2$CI_lower_bound <- ttest2$adj_diff - qt(1-0.05/2,GVT_table$shots_2ma + GVT_table$shots_2mi - 2)*ttest2$se2
ttest2$CI_upper_bound <- ttest2$adj_diff + qt(1-0.05/2,GVT_table$shots_2ma + GVT_table$shots_2mi - 2)*ttest2$se2
ttest2$se_lower <- ttest2$adj_diff - ttest2$se2
ttest2$se_upper <- ttest2$adj_diff + ttest2$se2


shooter_adj <- c(1:14, 1:12)
table2_thesis <- ttest2 %>%
  mutate(bias = bias2, `GVT est.` = `GVT est. k = 2`, `bias adj.` = adj_diff, shooter = shooter_adj) %>%
  select(shooter, `P(hit|2 makes)`, `P(hit)`, `P(hit|2 misses)`, `GVT est.`, bias, `bias adj.`)

#table output for latex
xtable(table2_thesis, digits = 3)




mean_adj_diff2 <- mean(ttest2$adj_diff)

#compute the total variance
total_var2 <- sum(ttest2$se2^2) #simulation: 0.6336564
#variance of the average difference across players
avg_var2 <- 1/(26^2) * total_var2 #simulation: 0.0009373616
std_err2 <- sqrt(avg_var2) #simulation: 0.03061636
t_value2 <- 1 - pnorm(mean_adj_diff2/std_err2) #simulation: 0.03958073





