setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Code/Bias (Sections 2 and 3)")
source("GVT_replication.R")

ttest4 <- GVT_output %>%
  select(shooter, `P(hit|4 makes)`, `P(hit)`, `P(hit|4 misses)`, `GVT est. k = 4`) %>%
  mutate(se4 = rep(NA, dim(GVT_output)[1]))

for(i in 1:(length(GVT_output$shooter))){
  #
  makes_after_three_makes <- GVT_table$FGM_4ma[i]
  misses_after_three_makes <- GVT_table$shots_4ma[i] - makes_after_three_makes
  sequence1 <- c(rep(1, makes_after_three_makes), rep(0, misses_after_three_makes))
  #
  makes_after_three_misses <- GVT_table$FGM_4mi[i]
  misses_after_three_misses <- GVT_table$shots_4mi[i] - makes_after_three_misses
  sequence2 <- c(rep(1, makes_after_three_misses), rep(0, misses_after_three_misses))
  #calculate pooled variance
  var1 <- var(sequence1)
  var2 <- var(sequence2)
  n1 <- length(sequence1)
  n2 <- length(sequence2)
  sdpool <- sqrt(((n1-1) * var1 + (n2 -1) * var2)/(n1+n2-2))
  ttest4$se4[i] <- sdpool * sqrt((1/n1) + (1/n2))
}
#import from Miller and Sanjurjo
ttest4$bias4 <- c(
  -.1737401, NA, -.1696316, -.1819515, -.1708156, -.1742681, -.2153843, -.2788394, -.177797, -.1729231, -.1725728, -.17331,
  -.1716182, -.1740733, -.1751149, -.1606337, -.1735801, -.1528182, -.1660514, -.1757661, -.1744444, -.1766239,
  -.1744225, -.1764417, -.1758117, NA
)

ttest4$adj_diff <- ttest4$`GVT est. k = 4` - ttest4$bias4
ttest4$CI_lower_bound <- ttest4$adj_diff - qt(1-0.05/2,GVT_table$shots_4ma + GVT_table$shots_4mi - 2)*ttest4$se4
ttest4$CI_upper_bound <- ttest4$adj_diff + qt(1-0.05/2,GVT_table$shots_4ma + GVT_table$shots_4mi - 2)*ttest4$se4
ttest4$se_lower <- ttest4$adj_diff - ttest4$se4
ttest4$se_upper <- ttest4$adj_diff + ttest4$se4

shooter_adj <- c(1:14, 1:12)
table4_thesis <- ttest4 %>%
  mutate(bias = bias4, `GVT est.` = `GVT est. k = 4`, `bias adj.` = adj_diff, shooter = shooter_adj) %>%
  select(shooter, `P(hit|4 makes)`, `P(hit)`, `P(hit|4 misses)`, `GVT est.`, bias, `bias adj.`)

#table output for latex
xtable(table4_thesis, digits = 3)

#normality checks
shapiro.test(ttest4$`GVT est. k = 4`)
shapiro.test(ttest4$adj_diff)
plot(density(na.omit(ttest4$`GVT est. k = 4`)))
plot(density(na.omit(ttest4$adj_diff)))



mean_adj_diff4 <- mean(ttest4$adj_diff[-c(2,26)]) #simulation: 0.1021076

ttest4_1 <- ttest4 %>% 
  na.omit()

#compute the total variance
total_var4 <- sum(ttest4_1$se4^2) #simulation: 2.085446
#variance of the average difference across players
avg_var4 <- 1/(21^2) * total_var4 #21 eligible players for computation, simulation: 0.004728902
std_err4 <- sqrt(avg_var4) #std error across 21 players, simulation: 0.06876701
p_value4 <- 1 - pnorm(mean_adj_diff4/std_err4) #MS 2018 round mean_adj_diff to 0.102 #mean across 24 players, simulation: 0.06879397

#look into this!
#compute t value with the mean of those players that were actually used to compute the std error
p_value4_alternative <- 1- pnorm(mean(ttest4_1$adj_diff)/std_err4) #simulation: 0.01511654
#this gives us a far better p-value


