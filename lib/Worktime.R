library(data.table)
library(plyr)
library(ggplot2)
library(plotly)

colstokeep_time <- c("MARHD", "MARHM", "JWAP", "JWDP", "SEX", "AEGP", "PERNP", "RAC1P", "ST")
T1 <- fread('ss13pusa.csv', select = colstokeep_time, na.strings = c("bbb", "bbbbbbb"))
T2 <- fread('ss13pusb.csv', select = colstokeep_time, na.strings = c("bbb", "bbbbbbb"))
Tfull <- rbind(T1, T2)
Tuse <- na.omit(Tfull)

Tuse$SEX <- as.factor(Tuse$SEX)

Tuse$JWAP_h <- ifelse(Tuse$JWAP %in% c(1:10), 0, 
                      ifelse(Tuse$JWAP %in% c(11,21), 1, 
                             ((Tuse$JWAP-22) %/% 12) + 2))

Tuse$JWDP_h <- ifelse(Tuse$JWDP %in% c(1:6), (Tuse$JWDP-1) %/% 2, 
               ifelse(Tuse$JWDP %in% c(7:18), (Tuse$JWDP-7) %/% 6 + 3, 
               ifelse(Tuse$JWDP %in% c(19:78), (Tuse$JWDP-19) %/% 12 + 5, 
               ifelse(Tuse$JWDP %in% c(79:132), (Tuse$JWDP-79) %/% 6 + 10, 
               ifelse(Tuse$JWDP %in% c(133:136), (Tuse$JWDP-133) %/% 2 + 19, 
               ifelse(Tuse$JWDP %in% c(137:148), (Tuse$JWDP-137) %/% 6 + 21, 23
               ))))))


findrate <- function(x) sum(x==1)/length(x)
rate_arr <- ddply(Tuse, .(JWAP_h, SEX), summarize, 
            n = length(MARHD), drate = findrate(MARHD), mrate = findrate(MARHM))


hour_sex_marry <- ggplot(rate_arr, aes(x = factor(JWAP_h), y = mrate, fill=SEX)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8)+  
  coord_polar(theta = "x", start = -0.13)+
  xlab("Time arrival at work") + ylab("Marriage rate")+
  labs(title = "Marriage rate vs Time arrival at work (in hour)")+
  scale_fill_discrete(labels=c("Male", "Female"))+
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        text = element_text(size=15))

hour_sex_marry  
  
hour_sex_divorce <- ggplot(rate_arr, aes(x = factor(JWAP_h), y = drate, fill=SEX)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8)+ 
  coord_polar(theta = "x", start = -0.13)+
  xlab("Time arrival at work") + ylab("Divorce rate")+
  labs(title = "Divorce rate vs Time arrival at work (in hour)")+
  scale_fill_discrete(labels=c("Male", "Female"))+
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        text = element_text(size=15))

hour_sex_divorce

###income

Tuse$earning <- ifelse(Tuse$PERNP %in% c(-9000:0), 0, 
                ifelse(Tuse$PERNP %in% c(0:104000), round(Tuse$PERNP/10000), 
                ifelse(Tuse$PERNP %in% c(104000:504000), "10-50", "50+")))
Tuse$earning <- factor(Tuse$earning, levels=c(as.character(0:10), "10-50", "50+"))

rate_earn <- ddply(Tuse, .(earning, SEX), summarize, 
                  n = length(MARHD), drate = findrate(MARHD), mrate = findrate(MARHM))


earn_sex_marry <- ggplot(rate_earn, aes(x = as.factor(earning), y = mrate, 
                                          group = SEX, fill=SEX)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  xlab("Total person's earnings (in 10k dollars)") + ylab("Marriage rate")+
  scale_fill_discrete(labels=c("Male", "Female"))+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
earn_sex_marry

earn_sex_divorce <- ggplot(rate_earn, aes(x = as.factor(earning), y = drate, 
                                          group = SEX, fill=SEX)) +
  geom_bar(stat = "identity", position = "dodge")+ 
  xlab("Total person's earnings (in 10k dollars)") + ylab("Divorce rate")+
  scale_fill_discrete(labels=c("Male", "Female"))+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
earn_sex_divorce
  
rate_earn_transfer <- rbind(data.frame(type="marry", rate_earn[, c(1:3)], rate=rate_earn[,5]),
                            data.frame(type="divorce", rate_earn[, c(1:3)], rate=-rate_earn[,4]))

earn_sex <- ggplot(rate_earn_transfer, 
                   aes(x = earning, y = rate, group = SEX, fill = SEX)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8)+ 
  geom_hline(yintercept = 0)+
  xlab("Total person's earnings (in 10k dollars)") + ylab("Marriage and Divorce rate")+
  labs(title = "Marriage and Divorce rate vs Total person's earning (in $10k)")+
  scale_fill_discrete(labels=c("Male", "Female"))+
  theme(legend.justification=c(1,1), legend.position=c(1,1), 
        text = element_text(size=15))
earn_sex

###race region
Tuse$race <- as.factor(ifelse(Tuse$RAC1P %in% c(1,2,6,9), Tuse$RAC1P, 
             ifelse(Tuse$RAC1P %in% c(3,4,5), 3, 7)))

levels(Tuse$race) <- c("White", "Black or African American", 
                "American Indian or Alaska Native", "Asian", "Some Other Race", 
                "Two or More Races")

Tuse$state <- as.factor(ifelse(Tuse$ST == 11, 24, Tuse$ST))

levels(Tuse$state) <- state.region

rate_race <- ddply(Tuse, .(race, state), summarize, 
             n = length(MARHD), drate = findrate(MARHD), mrate = findrate(MARHM))
rate_race$"sq_n" <- sqrt(rate_race$n)
names(rate_race)[4] <- "divorce_rate"
names(rate_race)[5] <- "marriage_rate"


plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)

p <- plot_ly(rate_race, x = divorce_rate, y = marriage_rate, text = paste("Division: ", state), 
        mode = "markers", color = race, size = sq_n)
p %>% layout(legend = list(x = 0, y = 1))


