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

wdi <- wdi %>% gather(year,values,c(3:ncol(wdi)))
wdi <- wdi  %>% spread(Indicator.Code, values) 

missing <- wdi %>% summarize_all(funs(sum(is.na(.))/n()))
missing <- gather(missing, key="feature", value="missing_pct")
goodv <- filter(missing, missing_pct<0.25)

wdi_good <- wdi %>% select(goodv$feature)

wdi_good %>% write_csv('wdi_good.csv')

footnote <- data.frame(read_excel('W03b_wdi.xlsx', sheet = 'FootNote'))
footnote1 <- footnote %>% subset(footnote$SeriesCode %in% goodv$feature)
footnote1 %>% write_csv('footnote_good.csv')
