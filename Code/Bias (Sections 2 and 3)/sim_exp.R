library(Cairo)

## For streaks of length 1:
nsims = 5000
n = 3
p = .5

results = rep(NA,nsims)
for (sim in 1:nsims) {
  tosses = rbinom(n,1,p)
  candidates = which(tosses == 1) + 1
  observed_candidates = candidates[candidates <= n]
  results[sim] = sum(tosses[observed_candidates])/length(observed_candidates)
}
expected_heads = mean(na.omit(results))
expected_heads

## For more general streaks
get_expected_heads = function(n,nsims = 1000,p=.5,streak=1) {
  results = rep(NA,nsims)
  for (sim in 1:nsims) {
    tosses = rbinom(n,1,p)
    runs = rle(tosses)
    n_tails_after = length(which(runs$values == 1 & runs$lengths >=streak))
    n_heads_after = sum(runs$lengths[which(runs$values == 1 & runs$lengths >=streak)]-streak)
    
    # Account for edge case: 
    if (n %in% cumsum(runs$lengths)[which(runs$values == 1 & runs$lengths >=streak)]) {
      n_tails_after = n_tails_after -1
    }
    
    results[sim] = n_heads_after/(n_heads_after+n_tails_after)
  }
  expected_heads = mean(na.omit(results))
  return(expected_heads)
}



## Graph with changing p
max_N = 50
x = 1:max_N
y.5 = rep(NA,length(x))
y.25 = rep(NA,length(x))
y.75 = rep(NA,length(x))

for (n in 1:(length(x))) {
  y.5[n] = get_expected_heads(x[n],nsims = 5000,p=.5,streak=1)
  y.25[n] = get_expected_heads(x[n],nsims = 5000,p=.25,streak=1)
  y.75[n] = get_expected_heads(x[n],nsims = 5000,p=.75,streak=1)
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
  y_3[n] = get_expected_heads(x[n],nsims = 50000,p=.75,streak=3)
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
