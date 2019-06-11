setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Hot-Hand/Code/Bias (Sections 2 and 3)")
source("GVT_replication.R")

#create 50000 permutations
for(i in 1:length(unique(rawdata$sid))){
  temp <- rawdata %>% 
    filter(sid == unique(rawdata$sid)[i])
  mat <- matrix(NA, ncol = length(temp$make), nrow = 50000)
  for(j in 1:50000){
    mat[j,] <- sample(temp$make)
  }
  assign(paste('permutations', unique(rawdata$sid)[i], sep = ""), mat)
}
rpermutations102
