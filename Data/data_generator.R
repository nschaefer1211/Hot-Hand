#This file is here to clean and generate data I use in the other code files.
setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Code/Bias (Sections 2 and 3)")
source("calc_exp.R")

#########################################################################################
############# Data to generate the graph success_prop.png in graphs_calc.R ##############
#########################################################################################

max_N = 100
x = 1:max_N
y_11 = rep(NA,length(x))
y_12 = rep(NA,length(x))
y_13 = rep(NA,length(x))
y_14 = rep(NA,length(x))
y_21 = rep(NA,length(x))
y_22 = rep(NA,length(x))
y_23 = rep(NA,length(x))
y_24 = rep(NA,length(x)) 
y_31 = rep(NA,length(x))
y_32 = rep(NA,length(x))
y_33 = rep(NA,length(x))
y_34 = rep(NA,length(x)) 

for (n in 1:(length(x))) {
  y_11[n] = exp_prop(N = x[n],k = 1, p = 0.5)
  y_12[n] = exp_prop(N = x[n],k = 2, p = 0.5)
  y_13[n] = exp_prop(N = x[n],k = 3, p = 0.5)
  y_14[n] = exp_prop(N = x[n],k = 4, p = 0.5)
  y_21[n] = exp_prop(N = x[n],k = 1, p = 0.7)
  y_22[n] = exp_prop(N = x[n],k = 2, p = 0.7)
  y_23[n] = exp_prop(N = x[n],k = 3, p = 0.7)
  y_24[n] = exp_prop(N = x[n],k = 4, p = 0.7)
  y_31[n] = exp_prop(N = x[n],k = 1, p = 0.3)
  y_32[n] = exp_prop(N = x[n],k = 2, p = 0.3)
  y_33[n] = exp_prop(N = x[n],k = 3, p = 0.3)
  y_34[n] = exp_prop(N = x[n],k = 4, p = 0.3)
  
}

exp_success <- data.frame(exp_1_0.5 = y_11, exp_2_0.5 = y_12, exp_3_0.5 = y_13, exp_4_0.5 = y_14,
                 exp_1_0.7 = y_21, exp_2_0.7 = y_22, exp_3_0.7 = y_23, exp_4_0.7 = y_24,
                 exp_1_0.3 = y_31, exp_2_0.3 = y_32, exp_3_0.3 = y_33, exp_4_0.3 = y_34)
save(exp_success, file = "exp_success_prop.Rdata")

#####################################################################################
############# Data to generate the graph symm.png in graphs_calc.R ##################
#####################################################################################

max_N = 100
x = 1:max_N
y_11 = rep(NA,length(x))
y_12 = rep(NA,length(x))
y_13 = rep(NA,length(x))
y_14 = rep(NA,length(x))
y_21 = rep(NA,length(x))
y_22 = rep(NA,length(x))
y_23 = rep(NA,length(x))
y_24 = rep(NA,length(x)) 

for (n in 1:(length(x))) {
  y_11[n] = exp_prop(N = x[n],k = 1, p = 0.5)
  y_12[n] = exp_prop(N = x[n],k = 2, p = 0.5)
  y_13[n] = exp_prop(N = x[n],k = 3, p = 0.5)
  y_14[n] = exp_prop(N = x[n],k = 4, p = 0.5)
  y_21[n] = exp_prop2(N = x[n],k = 1, p = 0.5)
  y_22[n] = exp_prop2(N = x[n],k = 2, p = 0.5)
  y_23[n] = exp_prop2(N = x[n],k = 3, p = 0.5)
  y_24[n] = exp_prop2(N = x[n],k = 4, p = 0.5)
}

symm <- data.frame(exp_1_0.5 = y_11, exp_2_0.5 = y_12, exp_3_0.5 = y_13, exp_4_0.5 = y_14,
                   exp2_1_0.5 = y_21, exp2_2_0.5 = y_22, exp2_3_0.5 = y_23, exp2_4_0.5 = y_24)
save(symm, file = "symmetry.Rdata")




#####################################################################################
############## Data to generate the graph diff_prop.png in graphs_calc.R#############
#####################################################################################
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


diff <- data.frame(diff_1_0.5 = y_11, diff_2_0.5 = y_21, diff_3_0.5 = y_31, diff_3_0.25 = y_41, diff_3_0.6 = y_51)
save(diff, file = "diff.Rdata")


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

for(i in 1:length(p_hit)){
  b_3[i] <- exp_diff(N = n[i], k = 3, p = p_hit[i])
}

#bias with streak = 4
b_4 <- rep(NA, length(p_hit))

for(i in 1:length(p_hit)){
  b_4[i] <- exp_diff(N = n[i], k = 4, p = p_hit[i])
}

b_2 <- bias$bias2
b_3 <- bias$bias3
bias <- data.frame(bias2 = b_2, bias3 = b_3, bias4 = b_4)
save(bias, file = "bias.Rdata")

