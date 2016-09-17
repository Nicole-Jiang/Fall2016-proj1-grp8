library(data.table)
colstokeep_pus <- c("AGEP", "MAR", "SCHL", "FOD1P", "SCIENGRLP")
dataset_a <- fread('csv_pus/ss14pusa.csv', select= colstokeep_pus)
dataset_b <- fread('csv_pus/ss14pusb.csv', select= colstokeep_pus)
data.fm<- data.frame(rbind(dataset_a, dataset_b)[AGEP >18,])

#names(data.fm)
hist(data.fm[,2])

> nrow(subset(data.fm, MAR == 5)) / nrow(data.fm) # never married people perportion
