setwd("~/Documents/Courses/ADS/Project 1")
# libraries we need
# install necessary package
#install.packages('devtools')
#install.packages("FactoMineR")
#install.packages("gplots")
#devtools::install_github("kassambara/factoextra")
library("FactoMineR")
library("factoextra")
library("gplots")
library("data.table")

# data cleansing
colstokeep_pus <- c("AGEP", "MAR", "MARHT", "SCHL", "FOD1P", "SCIENGRLP")
dataset_a <- fread('csv_pus/ss14pusa.csv', select= colstokeep_pus)
dataset_b <- fread('csv_pus/ss14pusb.csv', select= colstokeep_pus)
data.fm<- data.frame(rbind(dataset_a, dataset_b)[AGEP >18,])
data.fm[is.na(data.fm)] <- 0

#nrow(subset(data.fm, MAR == 5)) / nrow(data.fm) # never married people perportion

#####################################################
# education level calculation
#single_lowedu= nrow(subset(data.fm, MAR==5&SCHL<15))
#single_highschool= nrow(subset(data.fm, MAR==5 & SCHL> 16& SCHL< 20))
#single_bachelor = 
lowedu= subset(data.fm, SCHL<=15) #lower than high school
highschool= subset(data.fm, SCHL>= 16 & SCHL<= 20) #including high school, GED, Associate's degree or enter the college without degree
bachelor= subset(data.fm, SCHL==21) 
master= subset(data.fm, SCHL==22| SCHL ==23) #including master and professional degree
phd= subset(data.fm, SCHL==24)
#dim(master)
data_edu= list(lowedu,highschool,bachelor, master, phd)

#####################################################
#generate marry-education matrix
mar_edu= matrix(nrow=5,ncol=4)
rownames(mar_edu) <- c("Low education", "High school", "Bachelor", "Master", "PhD")
colnames(mar_edu) <- c("Never", "Once", "Twice", ">3 times")
for(i in 1:5){
  for(j in 1:4){
    mar_edu[i,j]= nrow(subset(data_edu[[i]],MARHT == j-1))
  }
}

#####################################################
#draw the balloonplot
balloonplot(t(as.table(mar_edu)), main ="Marriage and Education level", xlab ="Marriage", ylab="Education level",
             dotsize = 8, text.size= 0.7, label = FALSE, show.margins = FALSE)
#http://www.sthda.com/english/wiki/correspondence-analysis-in-r-the-ultimate-guide-for-the-analysis-
#the-visualization-and-the-interpretation-r-software-and-data-mining#at_pco=smlwn-1.0&at_si=57dd7e521
#65dfc04&at_ab=per-2&at_pos=0&at_tot=1

#####################################################
#draw the sunburst plot
#devtools::install_github
#devtools::install_github("timelyportfolio/sunburstR")
library(sunburstR)
library(TraMineR)
library(pipeR)
library(RColorBrewer)
library(stringr)

edu= subset(data.fm, select= c("MARHT","SCHL","SCIENGRLP"))
#edu[,1]= as.character(edu[,1])
edu[,1]= sapply(edu[,1], paste, "time(s)")
#sci <- function(x){ if(x==1) return ("STEM") else if(x==2) return("Non-STEM")}
#edu[,3]=sapply(edu[,3],sci)
edu_test= edu[1:10000,] ### 数据量太大，先跑10000行，之后再说吧
#edu_test[,3]=apply(edu_test[,3],1,sci)

for(i in 1:nrow(edu_test)){
  if(edu_test[i,2]<15){
    edu_test[i,2]="Lowedu"
  }else if(edu_test[i,2]>=16 & edu_test[i,2]<=20){
    edu_test[i,2]="Highschool"
  }else if(edu_test[i,2]==21){
    edu_test[i,2]="Bachelor"
  }else if(edu_test[i,2]==22|edu_test[i,2]==23){
    edu_test[i,2]="Master"
  }else{
    edu_test[i,2]="PhD"
  }
  #if(i%%1000==0){
  #  print(i)
  #}
}
#prepare for sunburst plot
edu.seq <- seqdef(edu_test)
sun_edu=seqtab(edu.seq, tlim = 0, format = "STS" )
name=names(attributes(as.list(sun_edu))$weights)
freq=as.numeric(attributes(as.list(sun_edu))$freq[,2])
sun_edu.fm=data.frame(name,freq)

###
sun_edu.fm[,1]= as.character(sun_edu.fm[,1])
sun_edu.fm[,1]= str_replace_all(sun_edu.fm[,1],"-0"," ")
sun_edu.fm[,1]= str_replace_all(sun_edu.fm[,1],"-1","-STEM")
sun_edu.fm[,1]= str_replace_all(sun_edu.fm[,1],"-2","-Non_STEM")
#sun_edu.fm[1,1]= "1 time(s)-Highschool"
#sun_edu.fm[2,1]= "0 time(s)-Highschool"
###


cols=brewer.pal(11,"Set3")
sunburst(sun_edu.fm,colors=cols)

