# This file produces the figures that directly show the bias as a function of trial length (Figure 1, 2, and 5)
# The graphs are also visible in the Graph folder of this repository

library(Cairo)
source("calc_exp.R")

########################################################################################################
################ Graph with changing streak length k = 1, 2, 3, 4 and p = 0.3, 0.5, and 0.7 ############
########################################################################################################
# load data generated in data.generator.R
load("exp_success_prop.Rdata") # make sure to have the correct working directory

## Plot and save picture
x <- 1:100
png(
  filename = "success_prop.png",
  type = "cairo",
  units = "in",
  width = 6,
  height = 5,
  pointsize = 12,
  res = 250
)
plot(x, exp_success$exp_1_0.5, type = "l", xlab = "Number of shots (n)", ylab = "Expected proportion of successes", col = "gray90", ylim = c(0.1, .85), lwd = 2)
lines(x, exp_success$exp_2_0.5, col = "gray60", lwd = 2)
lines(x, exp_success$exp_3_0.5, col = "gray30", lwd = 2)
lines(x, exp_success$exp_4_0.5, col = "gray0", lwd = 2)
lines(x, exp_success$exp_1_0.7, col = "gray90", lwd = 2)
lines(x, exp_success$exp_2_0.7, col = "gray60", lwd = 2)
lines(x, exp_success$exp_3_0.7, col = "gray30", lwd = 2)
lines(x, exp_success$exp_4_0.7, col = "gray0", lwd = 2)
lines(x, exp_success$exp_1_0.3, col = "gray90", lwd = 2)
lines(x, exp_success$exp_2_0.3, col = "gray60", lwd = 2)
lines(x, exp_success$exp_3_0.3, col = "gray30", lwd = 2)
lines(x, exp_success$exp_4_0.3, col = "gray0", lwd = 2)
lines(x, rep(0.7, 100), lty = "dashed", col = "black", lwd = 2)
lines(x, rep(0.5, 100), lty = "dashed", col = "black", lwd = 2)
lines(x, rep(0.3, 100), lty = "dashed", col = "black", lwd = 2)
text(50, .61, "p = .7")
text(50, .445, "p = .5")
text(50, .26, "p = .3")
legend(21, .83,
  legend = c("k = 1", "k = 2", "k = 3", "k = 4"), col = c("gray90", "gray60", "gray30", "gray0"),
  lwd = c(2, 2, 2, 2), lty = c(1, 1, 1, 1), cex = 0.8, horiz = TRUE
)
dev.off()

######################################################################
############## Graph to show the symmetry ############################
######################################################################
# load data generated in data_generator.R
load("symmetry.Rdata") # make sure to have the correct working directory

x <- 1:100
## Plot and save picture
png(
  filename = "success_prop_comp.png",
  type = "cairo",
  units = "in",
  width = 6,
  height = 5,
  pointsize = 12,
  res = 250
)
plot(x, symm$exp_1_0.5, type = "l", xlab = "Number of shots (n)", ylab = "Expected proportion of successes", col = "gray90", ylim = c(.19, .7), lwd = 2)
lines(x, symm$exp_2_0.5, col = "gray60", lwd = 2)
lines(x, symm$exp_3_0.5, col = "gray30", lwd = 2)
lines(x, symm$exp_4_0.5, col = "gray0", lwd = 2)
lines(x, symm$exp2_1_0.5, col = "gray90", lwd = 2)
lines(x, symm$exp2_2_0.5, col = "gray60", lwd = 2)
lines(x, symm$exp2_3_0.5, col = "gray30", lwd = 2)
lines(x, symm$exp2_4_0.5, col = "gray0", lwd = 2)
lines(x, rep(0.5, 100), lty = "dashed", col = "black", lwd = 2)
legend(13, .3, c("k = 1", "k = 2", "k = 3", "k = 4"),
  col = c("gray90", "gray60", "gray30", "gray0"), horiz = TRUE,
  lwd = c(2, 2, 2, 2), lty = c(1, 1, 1, 1)
)
dev.off()





########################################################################
############## EXpected Difference in Proportions ######################
########################################################################
# load data generated in data.generator.R
load("diff.Rdata") # make sure to have the correct working directory

# To save some calculation time, between n = 20 and n = 86 I have only calculated every second shot length
# between 87 and 98 only every third
x1 <- 1:20
x2 <- c(21:86)[c(FALSE, TRUE)]
x3 <- c(87:98)[c(FALSE, FALSE, FALSE, TRUE)]
x <- c(x1, x2, x3, 100)
## Plot and save picture
png(
  filename = "diff_prop.png",
  type = "cairo",
  units = "in",
  width = 6,
  height = 5,
  pointsize = 12,
  res = 250
)
plot(x, diff$diff_1_0.5, type = "l", xlab = "Number of shots (n)", ylab = "Expected difference", col = "gray47", ylim = c(-0.5, 0), lwd = 2)
lines(x, diff$diff_2_0.5, col = "gray87", lwd = 2)
lines(x, diff$diff_3_0.5, col = "black", lwd = 2)
lines(x, diff$diff_3_0.25, col = "black", lty = 3, lwd = 2)
lines(x, diff$diff_3_0.6, col = "black", lty = 2, lwd = 2)
legend(65, -.3,
  legend = c("k = 1, p = .5", "k = 2, p = .5", "k = 3, p = .5", "k = 3, p = .75, .25", "k = 3, p = .6"),
  col = c("gray47", "gray87", "black", "black", "black"), lwd = c(2, 2, 2, 2, 2), lty = c(1, 1, 1, 3, 2), cex = 0.85
)
dev.off()
