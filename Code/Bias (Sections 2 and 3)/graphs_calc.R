library(Cairo)
source("calc_exp.R")

## Graph with changing p
max_N = 50
x = 1:max_N
y.5 = rep(NA,length(x))
y.25 = rep(NA,length(x))
y.75 = rep(NA,length(x))

for (n in 1:(length(x))) {
  y.5[n] = exp_prop(N = x[n],k = 1, p = 0.5)
  y.25[n] = exp_prop(N = x[n],k = 1, p = 0.25)
  y.75[n] = exp_prop(N = x[n],k = 1, p = 0.75)
}

## Plot and save picture
png(filename="head_prop_p.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=5, 
    pointsize=12, 
    res=250)
plot(x,y.25,type="l",xlab="n",ylab="Expected proportion of heads",col="red",ylim=c(.00,.8))
lines(x,y.5,col="blue")
lines(x,y.75,col="dark green")
legend(max_N*.76,.2,c("p=0.75","p=0.50","p=0.25"),lwd=c(1,1),col=c("dark green","blue","red"))
dev.off()

## Graph with changing streak length k
max_N = 100
x = 1:max_N
y_1 = rep(NA,length(x))
y_2 = rep(NA,length(x))
y_3 = rep(NA,length(x))

for (n in 1:(length(x))) {
  y_1[n] = exp_prop(N = x[n],k = 1, p = 0.5)
  y_2[n] = exp_prop(N = x[n],k = 2, p = 0.5)
  y_3[n] = exp_prop(N = x[n],k = 3, p = 0.5)
}

## Plot and save picture
png(filename="head_prop_k.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=5, 
    pointsize=12, 
    res=250)
plot(x,y_1,type="l",xlab="n",ylab="Expected proportion of heads",col="red",ylim=c(.3,.56))
lines(x,y_2,col="blue")
lines(x,y_3,col="dark green")
legend(max_N*.811,.365,c("k=1","k=2","k=3"),lwd=c(1,1),col=c("red","blue","dark green"))
dev.off()

even <- function(x){
  if(x %% 2 == 0){
    return(x)
  }
  else{}
} 
even(1:10)

max_N <- 70
x <- 61:max_N
x <- x[c(FALSE, TRUE)]
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


setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Data")
load("diff.Rdata")

y_1 <- c(df$diff_1_0.5, y_11)
y_2 <- c(df$diff_2_0.5, y_21)
y_3 <- c(df$diff_3_0.5, y_31)
y_4 <- c(df$diff_3_0.25, y_41)
y_5 <- c(df$diff_3_0.6, y_51)

#for safety reason different name
df1 <- data.frame(diff_1_0.5 = y_1, diff_2_0.5 = y_2, diff_3_0.5 = y_3, diff_3_0.25 = y_4, diff_3_0.6 = y_5)
save(df, file = "diff.Rdata")

x1 <- 1:20 
x2 <- c(21:70)[c(FALSE, TRUE)]
x <- c(x1, x2)
## Plot and save picture
png(filename="diff_prop_k.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=5, 
    pointsize=12, 
    res=250)
plot(x,df$diff_1_0.5,type="l",xlab="n",ylab="Expected difference",col="red",ylim=c(-0.5,0))
lines(x,df$diff_2_0.5,col="blue")
lines(x,df$diff_3_0.5,col="dark green")
lines(x,df$diff_3_0.25, col = "yellow")
lines(x, df$diff_3_0.6, col = "purple", lty = 2)
#legend(max_N*.811,.365,c("k=1","k=2","k=3"),lwd=c(1,1),col=c("red","blue","dark green", "yellow", ""))
dev.off()
