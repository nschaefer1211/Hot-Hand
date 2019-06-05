library(XLConnect)
library(dplyr)

setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Supplement-Data_Code_Statistics/0-RAWDATA")
wb <- loadWorkbook("GilovichValloneTversky--CognitivePsychology--1985_CornellData.xls")
rawdata <- readWorksheet(wb, sheet = "Sheet1", header = TRUE)


shooters <- unique(rawdata$sid)
GVT_table <- data.frame(
  shooter = c(), shots = c(), FGM = c(),
  shots_3ma = c(), FGM_3ma = c(), shots_3mi = c(), FGM_3mi = c(), shots_4ma = c(), FGM_4ma = c(),
  shots_4mi = c(), FGM_4mi = c()
)

for (j in 1:length(shooters)) {
  shooter_j <- rawdata %>%
    filter(sid == shooters[j]) 
  temp1 <- as.data.frame(matrix(ncol = length(names(rawdata)), nrow = 1000))
  colnames(temp1) <- colnames(rawdata)
  counter1 <- 0
  temp2 <- as.data.frame(matrix(ncol = length(names(rawdata)), nrow = 1000))
  colnames(temp2) <- colnames(rawdata)
  counter2 <- 0
  temp3 <- as.data.frame(matrix(ncol = length(names(rawdata)), nrow = 1000))
  colnames(temp3) <- colnames(rawdata)
  counter3 <- 0
  temp4 <- as.data.frame(matrix(ncol = length(names(rawdata)), nrow = 1000))
  colnames(temp4) <- colnames(rawdata)
  counter4 <- 0
    
  counter <- 0
  for (i in 1:length(shooter_j$make)) {
    if (counter >= 3) {
      counter1 <- counter1 + 1
      temp1[counter1, ] <- shooter_j[i, ]
    }
    if (shooter_j$make[i] == 1) {
      counter <- counter + 1
    }
    else {
      counter <- 0
    }
  }
    
  counter <- 0
  for (i in 1:length(shooter_j$make)) {
    if (counter >= 3) {
      counter2 <- counter2 + 1
      temp2[counter2, ] <- shooter_j[i, ]
    }
    if (shooter_j$make[i] == 0) {
      counter <- counter + 1
    }
    else {
      counter <- 0
    }
  }
    
  counter <- 0
  for (i in 1:length(shooter_j$make)) {
    if (counter >= 4) {
      counter3 <- counter3 + 1
      temp3[counter3, ] <- shooter_j[i, ]
    }
    if (shooter_j$make[i] == 1) {
      counter <- counter + 1
    }
    else {
      counter <- 0
    }
  }
    
  counter <- 0
  for (i in 1:length(shooter_j$make)) {
    if (counter >= 4) {
      counter4 <- counter4 + 1
      temp4[counter4, ] <- shooter_j[i, ]
    }
    if (shooter_j$make[i] == 0) {
      counter <- counter + 1
    }
    else {
      counter <- 0
    }
  }
  temp1 <- temp1 %>% na.omit()
  temp2 <- temp2 %>% na.omit()
  temp3 <- temp3 %>% na.omit()
  temp4 <- temp4 %>% na.omit()
  
  temp <- data.frame(
    shooter = c(shooters[j]),
    shots = c(length(shooter_j$make)), FGM = c(length((shooter_j %>% filter(make == 1))$make)),
    shots_3ma = c(length(temp1$make)), FGM_3ma = c(length((temp1 %>% filter(make == 1))$make)), 
    shots_3mi = c(length(temp2$make)), FGM_3mi = c(length((temp2 %>% filter(make == 1))$make)), 
    shots_4ma = c(length(temp3$make)), FGM_4ma = c(length((temp3 %>% filter(make == 1))$make)),
    shots_4mi = c(length(temp4$make)), FGM_4mi = c(length((temp4 %>% filter(make == 1))$make))
  )
  
  GVT_table <- rbind(GVT_table, temp)
}


GVT_final <- GVT_table %>%
  mutate(`FG%` = FGM/shots, `P(hit|3 makes)` = FGM_3ma/shots_3ma, `P(hit|3 misses)` = FGM_3mi/shots_3mi, `P(hit|4 makes)` = FGM_4ma/shots_4ma,
         `P(hit|4 misses)` = FGM_4mi/shots_4mi) %>%
  select(shooter, shots, `FG%`, shots_3ma, `P(hit|3 makes)`, shots_3mi, `P(hit|3 misses)`,
         shots_4ma, `P(hit|4 makes)`, shots_4mi, `P(hit|4 misses)`)

GVT_output <- GVT_final %>%
  select(shooter,`P(hit|4 misses)`, shots_4mi, `P(hit|3 misses)`, shots_3mi, `FG%`, shots, `P(hit|3 makes)`, shots_3ma, `P(hit|4 makes)`, shots_4ma) %>%
  mutate(`GVT est. k = 3` = round(`P(hit|3 makes)`-`P(hit|3 misses)`, digits = 3),
         `GVT est. k = 4` = round(`P(hit|4 makes)`-`P(hit|4 misses)`, digits = 3), 
         `P(hit|4 misses)` = paste(round(`P(hit|4 misses)`, digits = 3), paste("(", shots_4mi, ")", sep = "")),
         `P(hit|3 misses)` = paste(round(`P(hit|3 misses)`, digits = 3), paste("(", shots_3mi, ")", sep = "")),
         `FG%` = paste(round(`FG%`, digits = 3), paste("(", shots, ")", sep = "")),
         `P(hit|3 makes)` = paste(round(`P(hit|3 makes)`, digits = 3), paste("(", shots_3ma, ")", sep = "")),
         `P(hit|4 makes)` = paste(round(`P(hit|4 makes)`, digits = 3), paste("(", shots_4ma, ")", sep = ""))) %>%
  select(-shots_4mi, -shots_3mi, -shots, -shots_3ma, -shots_4ma)

library(xtable)
xtable(GVT_output, digits = 3)