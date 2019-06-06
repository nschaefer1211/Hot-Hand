setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Code/Bias (Sections 2 and 3)")
source("GVT_replication.R")

ttest <- GVT_output[-26] %>%
  mutate(se3 = rep(NA, dim(GVT_output)[1]))

for(i in 1:(length(GVT_output$shooter)-1)){
  #
  makes_after_three_makes <- GVT_table$FGM_3ma[i]
  misses_after_three_makes <- GVT_table$shots_3ma[i] - makes_after_three_makes
  sequence1 <- c(rep(1, makes_after_three_makes), rep(0, misses_after_three_makes))
  #
  makes_after_three_misses <- GVT_table$FGM_3mi[i]
  misses_after_three_misses <- GVT_table$shots_3mi[i] - makes_after_three_misses
  sequence2 <- c(rep(1, makes_after_three_misses), rep(0, misses_after_three_misses))
  #calculate pooled variance
  var1 <- var(sequence1)
  var2 <- var(sequence2)
  n1 <- length(sequence1)
  n2 <- length(sequence2)
  sdpool <- sqrt(((n1-1) * var1 + (n2 -1) * var2)/(n1+n2-2))
  ttest$se3[i] <- sdpool * sqrt((1/n1) + (1/n2))
}
#import from Miller and Sanjurjo
ttest$bias3 <- c(
  -.0812219, -.0990108, -.0898123, -.1059003, -.0863584, -.0848163, -.114956, -.1371056, -.0812219,
  -.0898123, -.0863584, -.0834327, -.0916617, -.0880336, -.0798501, -.1006367, -.0916617, -.1033653,
  -.0972603, -.0812219, -.0880336, -.0804257, -.0822288, -.0812219, -.0804257, NA
)

ttest$adj_diff <- ttest$`GVT est. k = 3` - ttest$bias3
ttest$CI_lower_bound <- ttest$adj_diff - qt(1-0.05/2,GVT_table$shots_3ma + GVT_table$shots_3mi - 2)*ttest$se3
ttest$CI_upper_bound <- ttest$adj_diff + qt(1-0.05/2,GVT_table$shots_3ma + GVT_table$shots_3mi - 2)*ttest$se3
ttest$se_lower <- ttest$adj_diff - ttest$se3
ttest$se_upper <- ttest$adj_diff + ttest$se3

