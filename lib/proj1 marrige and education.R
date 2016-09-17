library(data.table)
colstokeep_pus <- c("AGEP", "MAR", "SCHL", "FOD1P", "SCIENGRLP")
dataset_a <- fread('csv_pus/ss14pusa.csv', select= colstokeep)
dataset_b <- fread('csv_pus/ss14pusb.csv', select= colstokeep)
data.fm<- data.frame(rbind(dataset_a, dataset_b)[AGEP >21,])

#names(data.fm)
hist(data.fm[,2])
