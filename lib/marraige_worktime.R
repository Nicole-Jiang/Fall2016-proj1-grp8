library(data.table)
library(plyr)
library(ggplot2)

colstokeep_time <- c("MARHD", "MARHM", "MARHT", "JWAP", "JWDP", "SEX", "AEGP")
T1 <- fread('ss13pusa.csv', select = colstokeep_time, na.strings = "bbb")
T2 <- fread('ss13pusb.csv', select = colstokeep_time, na.strings = "bbb")
Tfull <- rbind(T1, T2)
Tuse <- na.omit(Tfull)

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

rate_dep <- ddply(Tuse, .(JWDP_h), summarize, 
            n = length(MARHD), drate = findrate(MARHD), mrate = findrate(MARHM))            

df <- expand.grid(hour = c(12,1:11), period = c("am", "pm"))
df <- cbind(df, rate_arr[2:4])


hour_plot <- ggplot(df, aes(x = factor(hour), y = mrate, fill = period)) +
  geom_bar(stat = "identity", position = "dodge")
hour_plot

hour_plot <- hour_plot + coord_polar(theta = "x", start = 0.26)
hour_plot

hour_plot <- hour_plot + coord_polar(theta = "x", start = 0.26)+
  xlab("")+ylab("")+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
        panel.background = element_blank(), panel.grid.major.x = element_line(colour="grey"),
        axis.text.x = element_text(size = 25), legend.title=element_blank())
hour_plot

hour_plot <- ggplot(rate_arr, aes(x = factor(JWAP_h), y = mrate, fill=as.factor(SEX))) +
  geom_bar(stat = "identity", position = "dodge")+ coord_polar(theta = "x", start = -0.15)
hour_plot


