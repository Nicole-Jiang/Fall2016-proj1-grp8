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
One_Tenth <- Income_sort[round(dim(Data)[1]/10)]
Two_Tenth <- Income_sort[round(dim(Data)[1]/10)*2]
Three_Tenth <- Income_sort[round(dim(Data)[1]/10)*3]
Four_Tenth <- Income_sort[round(dim(Data)[1]/10)*4]
Five_Tenth <- Income_sort[round(dim(Data)[1]/10)*5]
Six_Tenth <- Income_sort[round(dim(Data)[1]/10)*6]
Seven_Tenth <- Income_sort[round(dim(Data)[1]/10)*7]
Eight_Tenth <- Income_sort[round(dim(Data)[1]/10)*8]
Nine_Tenth <- Income_sort[round(dim(Data)[1]/10)*9]

#In general, the Category of Income is Upper Class, Middle Class and Lower Class.
#So, we divide the data into three parts by the sort of income
#Threat 1/3 and 2/3 of total income as split points.
Data$Income <- ifelse(Data$PINCP %in% c(min(Data$PINCP):One_Tenth), 1,
                      ifelse(Data$PINCP %in% c(One_Tenth:Two_Tenth), 2,
                             ifelse(Data$PINCP %in% c(Two_Tenth:Three_Tenth), 3,
                                    ifelse(Data$PINCP %in% c(Three_Tenth:Four_Tenth), 4,
                                           ifelse(Data$PINCP %in% c(Four_Tenth:Five_Tenth), 5,
                                                  ifelse(Data$PINCP %in% c(Five_Tenth:Six_Tenth), 6,
                                                         ifelse(Data$PINCP %in% c(Six_Tenth:Seven_Tenth), 7,
                                                                ifelse(Data$PINCP %in% c(Seven_Tenth:Eight_Tenth), 8,
                                                                      ifelse(Data$PINCP %in% c(Eight_Tenth:Nine_Tenth), 9,
                                                                             (10))))))))))



Marrate_plot <- ggplot(Data, aes(x = Income, y = PINCP, fill = Income)) + geom_bar(stat = "identity") + xlab("Income Level") + ylab("Total Income") + ggtitle("Total Income of different Income Level")
Marrate_plot

#More detail
Income_sort <- sort(Data$PINCP)
split_point <- NULL
split_point[1] <- Income_sort[1]
for(i in 2:101){
  split_point[i] <-Income_sort[round(dim(Data)[1]/100)*(i-1)] 
}

Data$Income <- ifelse(Data$PINCP %in% c(split_point[1]:split_point[2]), 1,
               ifelse(Data$PINCP %in% c(split_point[2]:split_point[3]), 2,
               ifelse(Data$PINCP %in% c(split_point[3]:split_point[4]), 3,
               ifelse(Data$PINCP %in% c(split_point[4]:split_point[5]), 4,
               ifelse(Data$PINCP %in% c(split_point[5]:split_point[6]), 5,
               ifelse(Data$PINCP %in% c(split_point[6]:split_point[7]), 6,
               ifelse(Data$PINCP %in% c(split_point[7]:split_point[8]), 7,
               ifelse(Data$PINCP %in% c(split_point[8]:split_point[9]), 8,
               ifelse(Data$PINCP %in% c(split_point[9]:split_point[10]), 9,
               ifelse(Data$PINCP %in% c(split_point[10]:split_point[11]), 10,
               ifelse(Data$PINCP %in% c(split_point[11]:split_point[12]), 11,
               ifelse(Data$PINCP %in% c(split_point[12]:split_point[13]), 12,
               ifelse(Data$PINCP %in% c(split_point[13]:split_point[14]), 13,
               ifelse(Data$PINCP %in% c(split_point[14]:split_point[15]), 14,
               ifelse(Data$PINCP %in% c(split_point[15]:split_point[16]), 15,
               ifelse(Data$PINCP %in% c(split_point[16]:split_point[17]), 16,
               ifelse(Data$PINCP %in% c(split_point[17]:split_point[18]), 17,
               ifelse(Data$PINCP %in% c(split_point[18]:split_point[19]), 18,
               ifelse(Data$PINCP %in% c(split_point[19]:split_point[20]), 19,
               ifelse(Data$PINCP %in% c(split_point[20]:split_point[21]), 20,
               ifelse(Data$PINCP %in% c(split_point[21]:split_point[22]), 21,
               ifelse(Data$PINCP %in% c(split_point[22]:split_point[23]), 22,
               ifelse(Data$PINCP %in% c(split_point[23]:split_point[24]), 23,
               ifelse(Data$PINCP %in% c(split_point[24]:split_point[25]), 24,
               ifelse(Data$PINCP %in% c(split_point[25]:split_point[26]), 25,
               ifelse(Data$PINCP %in% c(split_point[26]:split_point[27]), 26,
               ifelse(Data$PINCP %in% c(split_point[27]:split_point[28]), 27,
               ifelse(Data$PINCP %in% c(split_point[28]:split_point[29]), 28,
               ifelse(Data$PINCP %in% c(split_point[29]:split_point[30]), 29,
               ifelse(Data$PINCP %in% c(split_point[30]:split_point[31]), 30,
               ifelse(Data$PINCP %in% c(split_point[31]:split_point[32]), 31,
               ifelse(Data$PINCP %in% c(split_point[32]:split_point[33]), 32,
               ifelse(Data$PINCP %in% c(split_point[33]:split_point[34]), 33,
               ifelse(Data$PINCP %in% c(split_point[34]:split_point[35]), 34,
               ifelse(Data$PINCP %in% c(split_point[35]:split_point[36]), 35,
               ifelse(Data$PINCP %in% c(split_point[36]:split_point[37]), 36,
               ifelse(Data$PINCP %in% c(split_point[37]:split_point[38]), 37,
               ifelse(Data$PINCP %in% c(split_point[38]:split_point[39]), 38,
               ifelse(Data$PINCP %in% c(split_point[39]:split_point[40]), 39,
               ifelse(Data$PINCP %in% c(split_point[40]:split_point[41]), 40,
               ifelse(Data$PINCP %in% c(split_point[41]:split_point[42]), 41, 
               ifelse(Data$PINCP %in% c(split_point[42]:split_point[43]), 42,
               ifelse(Data$PINCP %in% c(split_point[43]:split_point[44]), 43,
               ifelse(Data$PINCP %in% c(split_point[44]:split_point[45]), 44,
               ifelse(Data$PINCP %in% c(split_point[45]:split_point[46]), 45,
               ifelse(Data$PINCP %in% c(split_point[46]:split_point[47]), 46,
               ifelse(Data$PINCP %in% c(split_point[47]:split_point[48]), 47,
               ifelse(Data$PINCP %in% c(split_point[48]:split_point[49]), 48,
               ifelse(Data$PINCP %in% c(split_point[49]:split_point[50]), 49,
               ifelse(Data$PINCP %in% c(split_point[50]:split_point[51]), 50,
               ifelse(Data$PINCP %in% c(split_point[51]:split_point[52]), 51,
               ifelse(Data$PINCP %in% c(split_point[22]:split_point[53]), 52,
               ifelse(Data$PINCP %in% c(split_point[53]:split_point[54]), 53,
               ifelse(Data$PINCP %in% c(split_point[54]:split_point[55]), 54,
               ifelse(Data$PINCP %in% c(split_point[55]:split_point[56]), 55,
               ifelse(Data$PINCP %in% c(split_point[56]:split_point[57]), 56,
               ifelse(Data$PINCP %in% c(split_point[57]:split_point[58]), 57,
               ifelse(Data$PINCP %in% c(split_point[58]:split_point[59]), 58,
               ifelse(Data$PINCP %in% c(split_point[59]:split_point[60]), 59,
               ifelse(Data$PINCP %in% c(split_point[60]:split_point[61]), 60,
               ifelse(Data$PINCP %in% c(split_point[61]:split_point[62]), 61,
               ifelse(Data$PINCP %in% c(split_point[62]:split_point[63]), 62,
               ifelse(Data$PINCP %in% c(split_point[63]:split_point[64]), 63,
               ifelse(Data$PINCP %in% c(split_point[64]:split_point[65]), 64,
               ifelse(Data$PINCP %in% c(split_point[65]:split_point[66]), 65,
               ifelse(Data$PINCP %in% c(split_point[66]:split_point[67]), 66,
               ifelse(Data$PINCP %in% c(split_point[67]:split_point[68]), 67,
               ifelse(Data$PINCP %in% c(split_point[68]:split_point[69]), 68,
               ifelse(Data$PINCP %in% c(split_point[69]:split_point[70]), 69,
               ifelse(Data$PINCP %in% c(split_point[70]:split_point[71]), 70,
               ifelse(Data$PINCP %in% c(split_point[71]:split_point[72]), 71,
               ifelse(Data$PINCP %in% c(split_point[72]:split_point[73]), 72,
               ifelse(Data$PINCP %in% c(split_point[73]:split_point[74]), 73,
               ifelse(Data$PINCP %in% c(split_point[74]:split_point[75]), 74,
               ifelse(Data$PINCP %in% c(split_point[75]:split_point[76]), 75,
               ifelse(Data$PINCP %in% c(split_point[76]:split_point[77]), 76,
               ifelse(Data$PINCP %in% c(split_point[77]:split_point[78]), 77,
               ifelse(Data$PINCP %in% c(split_point[78]:split_point[79]), 78,
               ifelse(Data$PINCP %in% c(split_point[79]:split_point[80]), 79,
               ifelse(Data$PINCP %in% c(split_point[80]:split_point[81]), 80,
               ifelse(Data$PINCP %in% c(split_point[81]:split_point[82]), 81,
               ifelse(Data$PINCP %in% c(split_point[82]:split_point[83]), 82,
               ifelse(Data$PINCP %in% c(split_point[83]:split_point[84]), 83,
               ifelse(Data$PINCP %in% c(split_point[84]:split_point[85]), 84,
               ifelse(Data$PINCP %in% c(split_point[85]:split_point[86]), 85,
               ifelse(Data$PINCP %in% c(split_point[86]:split_point[87]), 86,
               ifelse(Data$PINCP %in% c(split_point[87]:split_point[88]), 87,
               ifelse(Data$PINCP %in% c(split_point[88]:split_point[89]), 88,
               ifelse(Data$PINCP %in% c(split_point[89]:split_point[90]), 89,
               ifelse(Data$PINCP %in% c(split_point[90]:split_point[91]), 90,
               ifelse(Data$PINCP %in% c(split_point[91]:split_point[92]), 91,
               ifelse(Data$PINCP %in% c(split_point[92]:split_point[93]), 92,
               ifelse(Data$PINCP %in% c(split_point[93]:split_point[94]), 93,
               ifelse(Data$PINCP %in% c(split_point[94]:split_point[95]), 94,
               ifelse(Data$PINCP %in% c(split_point[95]:split_point[96]), 95,
               ifelse(Data$PINCP %in% c(split_point[96]:split_point[97]), 96,
               ifelse(Data$PINCP %in% c(split_point[97]:split_point[98]), 97,
               ifelse(Data$PINCP %in% c(split_point[98]:split_point[99]), 98,
               ifelse(Data$PINCP %in% c(split_point[99]:split_point[100]), 99,
               (100))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))



               
               
               