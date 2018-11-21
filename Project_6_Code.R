# Project 6 Alec Hon ------------------------------------------------------
# UCLA Data Science Course
setwd("~/Documents/UCLA_Data_Science_Course/Project_6")
library(readxl)
library(tidyverse)
library(dplyr)
library(caret)
library(corrplot)
library(leaps)
data <- data.frame(read_excel('W03b_wdi.xlsx'))
data1 <- data[, -c(which(colnames(data) == 'Indicator.Name'),which(colnames(data) == 'Country.Code'),which(colnames(data) == 'X1960'):which(colnames(data) == 'X1980'))]

poverty <- data1 %>% filter(Indicator.Code == "SI.POV.LMIC.GP")
pov12 <- poverty %>% filter(!X2012 %in% NA)
wdi <- data1 %>% filter(data$Country.Name %in% pov12$Country.Name)

wdi <- wdi %>% gather(year,values,c(3:ncol(wdi)))
wdi <- wdi  %>% spread(Indicator.Code, values) 

missing <- wdi %>% summarize_all(funs(sum(is.na(.))/n())) %>%  gather(key="feature", value="missing_pct")

goodv <- filter(missing, missing_pct<0.25)
goodv=rbind(goodv,c("SI.POV.LMIC.GP", 10))
wdi_good <- wdi %>% select(goodv$feature)



no_pov_na <- wdi_good[!is.na(wdi_good$SI.POV.LMIC.GP),]
test <- trial[c(which(trial$year == 'X2013'),which(trial$year == 'X2014'),which(trial$year == 'X2015'), which(trial$year == 'X2016')),]


table(trial$year)
trial2 <- trial[complete.cases(trial),]
trial3 <- trial2[-c(1,2)]

df2 <- cor(trial3)
hc <- findCorrelation(df2, cutoff=0.5) 
hc <- sort(hc)
hc <- hc[-match(which(colnames(trial3) == 'SI.POV.LMIC.GP'),hc)]
trial3.new = trial3[,-c(hc)]


subsets <- regsubsets(SI.POV.LMIC.GP~., data = trial3.new, nvmax = 10, really.big = T)
summary(subsets)  
plot(subsets, scale="adjr2")
plot(subsets, scale="bic")

plot(subsets$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(subsets$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

coef(subsets,10)
fit01 <- lm(SI.POV.LMIC.GP~ BM.GSR.TRVL.ZS+EN.ATM.CO2E.SF.ZS+FP.CPI.TOTL.ZG+SP.POP.2529.MA.5Y +SP.RUR.TOTL.ZG+TM.VAL.MRCH.OR.ZS+TM.VAL.MRCH.R6.ZS+TX.VAL.AGRI.ZS.UN+TX.VAL.TRAN.ZS.WT, data = trial3.new)
summary(fit01)

lmpred <- predict(fit01, newdata = test)
lmpred
rmse <-sqrt(mean((lmpred - test$SI.POV.LMIC.GP)^2, na.rm =T))


data %>% filter(Indicator.Code == 'TX.VAL.TRAN.ZS.WT')

