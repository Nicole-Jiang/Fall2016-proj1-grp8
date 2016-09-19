library(data.table)
library(readr)
library(plyr)
library(ggplot2)
library(dplyr)
library(foreach)
library(gridExtra)
setwd("/Users/kaishengwang/Desktop/Applied\ Data\ Science\ Project/Project1/data")
features <- c("MARHD", "MARHM", "PERNP", "PINCP", "AGEP", "SEX")
Data1 <- fread('ss13pusa.csv', select = features)
Data2 <- fread('ss13pusb.csv', select = features)
Data <- rbind(Data1, Data2)
Data <- na.omit(Data)
remove(Data1)
remove(Data2)
min(Data$PINCP)
#-13600
max(Data$PINCP)
#1281000
Income_sort <- sort(Data$PINCP)
One_Third <- Income_sort[round(dim(Data)[1]/3)]
Two_Third <- Income_sort[round(dim(Data)[1]/3)*2]
#In general, the Category of Income is Upper Class, Middle Class and Lower Class.
#So, we divide the data into three parts by the sort of income
#Threat 1/3 and 2/3 of total income as split points.
Data$Income <- ifelse(Data$PINCP %in% c(min(Data$PINCP):One_Third), "Low",
                      ifelse(Data$PINCP %in% c(One_Third:Two_Third), "Mid",
                             ("High")))
Mar_Lowin <- subset(Data, MARHM == 1 & Income == "Low")
Div_Lowin <- subset(Data, MARHD == 1 & Income == "Low")
Mar_Midin <- subset(Data, MARHM == 1 & Income == "Mid")
Div_Midin <- subset(Data, MARHD == 1 & Income == "Mid")
Mar_Highin <- subset(Data, MARHM == 1 & Income == "High")
Div_Highin <- subset(Data, MARHD == 1 & Income == "High")

Marrate_Lowin <- dim(Div_Lowin)[1]/dim(Mar_Lowin)[1]
Marrate_Midin <- dim(Div_Midin)[1]/dim(Mar_Midin)[1]
Marrate_Highin <- dim(Div_Highin)[1]/dim(Mar_Highin)[1]

Data$Marrate <- ifelse(Data$Income == "Low", Marrate_Lowin,
                       ifelse(Data$Income == "Mid", Marrate_Midin,
                              (Marrate_Highin)))
Data$Marrate <- round(Data$Marrate, digits = 2)
Marrate_plot <- ggplot(Data, aes(x = factor(Income), y = Marrate, fill = period)) + geom_bar(stat = "identity") + xlab("Income Level") + ylab("Rate of Divorce") + ggtitle("Rate of Divorce in different income-group")
#Marrate_plot <- ggplot(Data, aes(x = Income, y = PINCP, fill = Income)) + geom_bar(stat = "identity") + xlab("Income Level") + ylab("Total Income") + ggtitle("Total Income of different Income Level")
Marrate_plot

