source("sim_exp.R")

## Graph with changing p
max_N = 50
x = 1:max_N
y.5 = rep(NA,length(x))
y.25 = rep(NA,length(x))
y.75 = rep(NA,length(x))

for (n in 1:(length(x))) {
  y.5[n] = get_expected_heads(x[n],simtosses = 5000,p=.5,k=1)
  y.25[n] = get_expected_heads(x[n],simtosses = 5000,p=.25,k=1)
  y.75[n] = get_expected_heads(x[n],simtosses = 5000,p=.75,k=1)
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
max_N = 1000
x = 1:max_N
y_1 = rep(NA,length(x))
y_2 = rep(NA,length(x))
y_3 = rep(NA,length(x))

for (n in 1:(length(x))) {
  y_3[n] = get_expected_heads(x[n],simtosses = 50000,p=.75,k=3)
}

## Plot and save picture
png(filename="head_prop_k.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=5, 
    pointsize=12, 
    res=250)
plot(x,y_3,type="l",xlab="n",ylab="Expected proportion of heads",col="red",ylim=c(0.6,.8))
lines(x, rep(0.75, length(x)))
legend(max_N*.811,.365,c("k=1","k=2","k=3"),lwd=c(1,1),col=c("red","blue","dark green"))
dev.off()
