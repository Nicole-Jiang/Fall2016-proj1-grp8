library(data.table)
library(readr)
library(plyr)
library(ggplot2)
library(dplyr)
library(foreach)
library(gridExtra)
setwd("/Users/kaishengwang/Desktop/Applied\ Data\ Science\ Project/Project1/data")
features <- c("MARHD", "MARHM", "PERNP", "PINCP", "AGEP", "SEX", "MAR")
Data1 <- fread('ss13pusa.csv', select = features)
Data2 <- fread('ss13pusb.csv', select = features)
Data <- rbind(Data1, Data2)
Data <- na.omit(Data)
remove(Data1)
remove(Data2)
Data1 <- subset(Data, MAR != 5)

min(Data$PINCP)
#-13600
max(Data$PINCP)
#1281000

#In general, there are two kind of classification of income.
#One is Upper Class, Middle Class and Lower Class.
#Another is Upper Mid Class, Mid Class, Lower Mid Class and Lower Class
#So, Firstly, we divide the data into three parts by the sort of income
#Threat 1/3 and 2/3 of total income as split points.
Data$SEX <- as.factor(Data$SEX)
Income_sort <- sort(Data$PINCP)
One_Third <- Income_sort[round(dim(Data)[1]/3)]
Two_Third <- Income_sort[round(dim(Data)[1]/3)*2]

Data$Income <- ifelse(Data$PINCP %in% c(min(Data$PINCP):One_Third), "Low",
                      ifelse(Data$PINCP %in% c(One_Third:Two_Third), "Mid",
                             ("High")))
#findrate <- function(x) sum(x == 1)/length(x)
#Data2 <- ddply(Data, .(Income, SEX), summarise, n = length(MARHD), drate = findrate(MARHD), mrate = findrate(MARHM))
#In terms of Divorce rate, there are two kinds of ways to classicate. 
#One is the number of people who have been divorced divoice by the number of people who have been married.
#Another is the number of people who have been divorce divoice by the number of all population.
#I will utilize all of them in my code.
#Divrate1 = people who have been divorce or seperate / number of all people
Data_Divrate1 <- ddply(Data, .(Income, SEX), summarise, n = length(MARHD), Div_rate1 = (sum(MAR == 3) + sum(MAR == 4))/length(MAR))

Income_sex_Divrate1<- ggplot(Data_Divrate1, aes(x = factor(Income), y = Div_rate1, fill = SEX)) + 
                     geom_bar(stat = "identity", position = "dodge") + 
                     coord_polar(theta = "x") + 
                     xlab("Income Level") + ylab("Divorce rate") + 
                     scale_fill_discrete(labels = c("Male", "Female"))
Income_sex_Divrate1

#Divrate2 = poeple who have been divorce in the past 12 months / people who have been married in the past 12 months
Data_Divrate2 <- ddply(Data, .(Income, SEX), summarise, n = length(MARHD), Div_rate2 = (sum(MARHD == 1))/(sum(MARHM == 1)))

Income_sex_Divrate2 <- ggplot(Data_Divrate2, aes(x = factor(Income), y = Div_rate2, fill = SEX)) + 
                      geom_bar(stat = "identity", position = "dodge") + 
                      coord_polar(theta = "x") + 
                      xlab("Income Level") + ylab("Divorce rate") + 
                      scale_fill_discrete(labels = c("Male", "Female"))
Income_sex_Divrate2

# Another way to split the income level:
# Upper Mid Class > $100,000/Year
# $75,000/Year < Mid Class < $100,000/Year
# $35,000/Year < Lower Mid Class < $75,000/Year
# Lower Class < $35,000/Year
Income_sort <- sort(Data$PINCP)
Data$Income2 <- ifelse(Data$PINCP < 35000, "Low",
                      ifelse(Data$PINCP %in% c(35000:75000), "Lower Mid",
                             ifelse(Data$PINCP %in% c(75000: 100000), "Mid",
                                    ("Upper Mid"))))
#Divrate1 = people who have been divorce or seperate / number of all people
Data_Divrate1 <- ddply(Data, .(Income2, SEX), summarise, n = length(MARHD), Div_rate1 = (sum(MAR == 3) + sum(MAR == 4))/length(MAR))

Income2_sex_Divrate1<- ggplot(Data_Divrate1, aes(x = factor(Income2), y = Div_rate1, fill = SEX)) + 
                       geom_bar(stat = "identity", position = "dodge") + 
                       coord_polar(theta = "x") + 
                       xlab("Income Level") + ylab("Divorce rate") + 
                       scale_fill_discrete(labels = c("Male", "Female"))
Income2_sex_Divrate1

#Divrate2 = poeple who have been divorce in the past 12 months / people who have been married in the past 12 months
Data_Divrate2 <- ddply(Data, .(Income2, SEX), summarise, n = length(MARHD), Div_rate2 = (sum(MARHD == 1))/(sum(MARHM == 1)))

Income2_sex_Divrate2 <- ggplot(Data_Divrate2, aes(x = factor(Income2), y = Div_rate2, fill = SEX)) + 
                       geom_bar(stat = "identity", position = "dodge") + 
                       coord_polar(theta = "x") + 
                       xlab("Income Level") + ylab("Divorce rate") + 
                       scale_fill_discrete(labels = c("Male", "Female"))
Income2_sex_Divrate2

#We would like to use age as another split point.
#Becase we have already delete the people whose age is less than 16
#We will take seperate age into <30, 30:40, 40:50, 50:60, >60 
Income_sort <- sort(Data$PINCP)
One_Third <- Income_sort[round(dim(Data)[1]/3)]
Two_Third <- Income_sort[round(dim(Data)[1]/3)*2]

Data$Income <- ifelse(Data$PINCP %in% c(min(Data$PINCP):One_Third), "Low",
                      ifelse(Data$PINCP %in% c(One_Third:Two_Third), "Mid",
                             ("High")))
Data$Agegroup <- ifelse(Data$AGEP < 30, "less than 30",
                        ifelse(Data$AGEP %in% c(30:40), "30 to 40",
                               ifelse(Data$AGEP %in% c(40:50), "40 to 50",
                                      ifelse(Data$AGEP %in% c(50:60), "50 to 60",
                                             ("Large than 60")))))
#Divrate1 = people who have been divorce or seperate / number of all people
Data_Divrate1 <- ddply(Data, .(Income, Agegroup), summarise, n = length(MARHD), Div_rate1 = (sum(MAR == 3) + sum(MAR == 4))/length(MAR))

Income_sex_Divrate1<- ggplot(Data_Divrate1, aes(x = factor(Income), y = Div_rate1, fill = Agegroup)) + 
                      geom_bar(stat = "identity", position = "dodge") + 
                      coord_polar(theta = "x") + 
                      xlab("Income Level") + ylab("Divorce rate") + 
                      scale_fill_discrete(labels = c("Less than 30", "30 to 40", "40 to 50", "50 to 60", "Large than 60"))
Income_sex_Divrate1

Income_sex_Divrate1<- ggplot(Data_Divrate1, aes(x = factor(Agegroup), y = Div_rate1, fill = Income)) + 
                      geom_bar(stat = "identity", position = "dodge") + 
                      coord_polar(theta = "x") + 
                      xlab("Income Level") + ylab("Divorce rate") + 
                      scale_fill_discrete(labels = c("Low", "Mid", "High"))
Income_sex_Divrate1
