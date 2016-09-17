library(data.table)
library(plyr)

colstokeep_pus <- c("AGEP", "MAR", "SCHL", "FOD1P", "SCIENGRLP")
dataset_a <- fread('csv_pus/ss14pusa.csv', select= colstokeep_pus)
dataset_b <- fread('csv_pus/ss14pusb.csv', select= colstokeep_pus)
data.fm<- data.frame(rbind(dataset_a, dataset_b)[AGEP >18,])

colstokeep_time <- c("MARHD", "MARHM", "MARHT", "JWAP", "JWDP")
T1 <- fread('ss13pusa.csv', select = colstokeep_time, na.strings = "bbb")
T2 <- fread('ss13pusb.csv', select = colstokeep_time, na.strings = "bbb")
Tfull <- rbind(T1, T2)
Tuse <- na.omit(Tfull)

#names(data.fm)
hist(data.fm[,2])

nrow(subset(data.fm, MAR == 5)) / nrow(data.fm) # never married people perportion


T_arrival <- split(Tuse, Tuse$JWAP)
drate <- sapply(T_arrival, function(C) sum(C$MARHD==1)/nrow(C))
mrate <- sapply(T_arrival, function(C) sum(C$MARHM==1)/nrow(C))

findrate <- function(x) sum(x==1)/length(x)
rate <- ddply(Tuse, .(JWAP), summarize, 
             drate = findrate(MARHD), mrate = findrate(MARHM))
               
drate = sum(C$MARHD==1)/nrow(C)
               mrate = sum(C$MARHM==1)/nrow(C) 
               return(c(drate, mrate))
# polygon function
drawPoly <- function(x, y1, y2) {
  polygon(c(x, rev(x)), c(y1,  rev(y2)),
          border = NA, col = "#ff000050")
}

# plot
par(las = 1, fg = "grey")
with(rate, {
  plot(drate ~ arrivalTime, 
       ylim = range(c(q1, q5)), type = "n", 
       yaxt = "n", xaxt = "n",
       ylab = "", xlab = "")
  drawPoly(arrivalTime, q1, q5)
  drawPoly(arrivalTime, q2, q4)
  lines(q3 ~ arrivalTime, 
        col = "black", lwd = 2)
})

# decoration
axis(1, seq(0, 24, by = 4), paste0(seq(0, 24, by = 4), ":00"))
axis(2)
mtext("earnings (USD) vs. arrival time at work", 3, 0.5, adj = 0, cex = 2)
legend("topright", c("median", "0.25-0.75 quantile", "0.4-0.6 quantile"),
       fill = c(NA, "#ff000075", "#ff000050"), lty = c(1, NA, NA), border = NA,
       col = c("black", NA, NA), text.col = "black", bty = "n")


