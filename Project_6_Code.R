# Project 6 Alec Hon ------------------------------------------------------
# UCLA Data Science Course
setwd("~/Documents/UCLA_Data_Science_Course")
library(readxl)
library(tidyverse)
library(dplyr)

data <- data.frame(read_excel('W03b_wdi.xlsx'))
data <- data[, -c(which(colnames(data) == 'Indicator.Name'),which(colnames(data) == 'Country.Code'),which(colnames(data) == 'X1960'):which(colnames(data) == 'X1980'))]

poverty <- data %>% filter(Indicator.Code == "SI.POV.LMIC.GP")
pov12 <- poverty %>% filter(!X2012 %in% NA)
wdi <- data %>% filter(data$Country.Name %in% pov12$Country.Name)

trial <- wdi %>% gather(year,values,c(3:ncol(wdi)))

