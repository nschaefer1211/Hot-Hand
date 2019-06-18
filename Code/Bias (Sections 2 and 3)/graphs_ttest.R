# Graphs for confidence intervals and standard errors of t-tests.
library(Cairo)

# streak/k = 2
source("ttest_streak2.R") #be sure to have the correct working directory
#sorting data frame according the bias corrected difference in proportions (ascending)
ttest2_sort <- ttest2[order(ttest2$adj_diff),] 

Shooter <- 1:26
png(filename="bias_corr2.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=5, 
    pointsize=12, 
    res=250)
plot(Shooter, ttest2_sort$adj_diff * 100,  ylim = c(-100, 100), cex = 1.5, type = "p", pch = 16, col = "black")
segments(x0 = Shooter, x1 = Shooter, y0 = ttest2_sort$adj_diff* 100 - ttest2_sort$se2 * 100,
         y1 = ttest2_sort$adj_diff * 100 + ttest2_sort$se2 * 100, lwd = 3, col = "gray60")
segments(x0 = Shooter, x1 = Shooter, y0 = ttest2_sort$CI_lower_bound * 100, y1 = ttest2_sort$CI_upper_bound * 100, lwd = 1, col = "black")
legend(20,-90,c("+/- Std. Err.", "95 % CI"), col=c("gray60","black"), horiz = TRUE)
dev.off()



# streak/k = 3
source("ttest_streak3.R") #be sure to have the correct working directory

ttest3_sort <- ttest3[order(ttest3$adj_diff),] 

Shooter <- 1:25
png(filename="bias_corr3.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=5, 
    pointsize=12, 
    res=250)
plot(Shooter, ttest3_sort$adj_diff[-26] * 100,  ylim = c(-100, 100), cex = 1.5, type = "p", pch = 16, col = "black")
segments(x0 = Shooter, x1 = Shooter, y0 = ttest3_sort$adj_diff* 100 - ttest3_sort$se3 * 100,
         y1 = ttest3_sort$adj_diff * 100 + ttest3_sort$se3 * 100, lwd = 3, col = "gray60")
segments(x0 = Shooter, x1 = Shooter, y0 = ttest3_sort$CI_lower_bound * 100, y1 = ttest3_sort$CI_upper_bound * 100, lwd = 1, col = "black")
legend(20,-90,c("+/- Std. Err.", "95 % CI"), col=c("gray60","black"), horiz = TRUE)
dev.off()


# streak/k = 4

source("ttest_streak4.R") #be sure to have the correct working directory

ttest4_sort <- ttest4[order(ttest4$adj_diff),] 

Shooter <- 1:24
png(filename="bias_corr4.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=5, 
    pointsize=12, 
    res=250)
plot(Shooter, ttest4_sort$adj_diff[-c(2,26)] * 100,  ylim = c(-100, 100), cex = 1.5, type = "p", pch = 16, col = "black")
segments(x0 = Shooter, x1 = Shooter, y0 = ttest4_sort$adj_diff* 100 - ttest4_sort$se4 * 100,
         y1 = ttest4_sort$adj_diff * 100 + ttest4_sort$se4 * 100, lwd = 3, col = "gray60")
segments(x0 = Shooter, x1 = Shooter, y0 = ttest4_sort$CI_lower_bound * 100, y1 = ttest4_sort$CI_upper_bound * 100, lwd = 1, col = "black")
legend(20,-90,c("+/- Std. Err.", "95 % CI"), col=c("gray60","black"), horiz = TRUE)
dev.off()





