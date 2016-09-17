# data cleansing
library(data.table)
colstokeep_pus <- c("AGEP", "MAR", "MARHT", "SCHL", "FOD1P", "SCIENGRLP")
dataset_a <- fread('csv_pus/ss14pusa.csv', select= colstokeep_pus)
dataset_b <- fread('csv_pus/ss14pusb.csv', select= colstokeep_pus)
data.fm<- data.frame(rbind(dataset_a, dataset_b)[AGEP >18,])
data.fm[is.na(data.fm)] <- 0

#names(data.fm)
#hist(data.fm[,3])
#nrow(subset(data.fm, MAR == 5)) / nrow(data.fm) # never married people perportion

# install necessary package
#install.packages('devtools')
#install.packages("FactoMineR")
#install.packages("gplots")

# libraries we need
library("FactoMineR")
library("factoextra")
library("gplots")
devtools::install_github("kassambara/factoextra")

# education level calculation
#single_lowedu= nrow(subset(data.fm, MAR==5&SCHL<15))
#single_highschool= nrow(subset(data.fm, MAR==5 & SCHL> 16& SCHL< 20))
#single_bachelor = 
lowedu= subset(data.fm, SCHL<15) #lower than high school
highschool= subset(data.fm, SCHL> 16 & SCHL< 20) #including high school, GED, Associate's degree or enter the college without degree
bachelor= subset(data.fm, SCHL==21) 
master= subset(data.fm, SCHL==22| SCHL ==23) #including master and professional degree
phd= subset(data.fm, SCHL==24)
#dim(master)
data_edu= list(lowedu,highschool,bachelor, master, phd)

#generate marry-education matrix
mar_edu= matrix(nrow=5,ncol=4)
rownames(mar_edu) <- c("Low education", "High school", "Bachelor", "Master", "PhD")
colnames(mar_edu) <- c("Never married", "Once", "Twice", "More than 3 times")
for(i in 1:5){
  for(j in 1:4){
    mar_edu[i,j]= nrow(subset(data_edu[[i]],MARHT == j-1))
  }
}

balloonplot(t(as.table(mar_edu)), main ="marry and edu", xlab ="", ylab="", label = FALSE, show.margins = FALSE)






