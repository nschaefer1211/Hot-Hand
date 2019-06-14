#This file is here to clean and generate data I use in the other code files.
setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Code/Bias (Sections 2 and 3)")
source("calc_exp.R")


#data to generate the graph diff_prop_k in graphics.R

x1 <- 1:20 
x2 <- c(21:86)[c(FALSE, TRUE)]
x3 <- c(87:98)[c(FALSE, FALSE, FALSE, TRUE)]
x <- c(x1, x2, x3, 100)
y_11 = rep(NA,length(x))
y_21 = rep(NA,length(x))
y_31 = rep(NA,length(x))
y_41 = rep(NA,length(x))
y_51 = rep(NA,length(x))
#y_61 <- rep(NA, length(x))
#y_71 <- rep(NA, length(x))
#y_81 <- rep(NA, length(x))

for (n in 1:(length(x))) {
  y_11[n] <- exp_diff(N = x[n],k = 1, p = 0.5)
  y_21[n] <- exp_diff(N = x[n],k = 2, p = 0.5)
  y_31[n] <- exp_diff(N = x[n],k = 3, p = 0.5)
  y_41[n] <- exp_diff(N = x[n], k = 3, p = 0.25)
  y_51[n] <- exp_diff(N = x[n], k = 3, p = 0.6)
}



#for(n in 1:(length(x))){
#  y_61[n] <- exp_prop(N = x[n], k = 1, p = 0.5) - exp_prop2(N = x[n], k = 1, p = 0.5)
#  y_71[n] <- exp_prop(N = x[n], k = 3, p = 0.5) - exp_prop2(N = x[n], k = 3, p = 0.5)
#  y_81[n] <- exp_prop(N = x[n], k = 3, p = 0.6) - exp_prop2(N = x[n], k = 3, p = 0.6)
#}


df <- data.frame(diff_1_0.5 = y_11, diff_2_0.5 = y_21, diff_3_0.5 = y_31, diff_3_0.25 = y_41, diff_3_0.6 = y_51)
save(df, file = "diff.Rdata")


#generating the bias for ttest.R

p_hit <- c(.54, .35, .60, .40, .42, .57, .56, .50, .54, .60, .58, .44, .61,
           .59, .48, .34, .39, .32, .36, .46, .41, .53, .45, .46, .53, .25)
n <- c(100, 100, 100, 90, 100, 100, 75, 50, rep(100, 18))
#bias with streak = 2
b_2 <- rep(NA, length(p_hit))

for(i in 1:length(p_hit)){
  b_2[i] <- exp_diff(N = n[i], k = 2, p = p_hit[i])
}

#bias with streak = 3
b_3 <- rep(NA, length(p_hit))

for(i in 1:5){
  b_3[i] <- exp_diff(N = n[i], k = 3, p = p_hit[i])
}

#bias with streak = 4
b_4 <- rep(NA, length(p_hit))

for(i in 1:length(p_hit)){
  b_4[i] <- exp_diff(N = n[i], k = 4, p = p_hit[i])
}

b_2 <- bias$bias2
bias <- data.frame(bias2 = b_2, bias3 = b_3, bias4 = b_4)
save(bias, file = "bias.Rdata")

