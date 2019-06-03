rm(list = ls())
library(devtools)
library(rJava)
library(tabulizer)
library(dplyr)
library(magrittr)

library(remotes)
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))

setwd("C:/Users/nscha/OneDrive/Studium/Bachelorarbeit VWL/Thesis VWL/Daten/2014-19/all")

data <- extract_tables("ATL1419ALL.pdf")[[2]]
names <- trimws(paste(data[1,], data[2,], data[3,], data[4,]))
ATL1419ALL <- as.data.frame(t(as.numeric(sub("%", "", as.character(data[5,]))))) %>%
  set_colnames(names) %>%
  mutate(FGM = FG2M + FG3M, FGA = FG2A + FG3A, `FG%` = round(FGM/FGA * 100, digits = 2))


