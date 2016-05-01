library(zoo)
library(plyr)
library(ggplot2)
library(reshape2)
library(leaps)
library(MASS)
library("e1071")
options(scipen=999)

import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE))
}

write.csv <- function(ob,filename){
  write.table(ob, filename, quote=FALSE, sep=",", row.names=FALSE)
}

my.data3 <- import.csv("pca_energy_FINAL.csv")

RMSE1<-as.numeric()
RMSE2<-as.numeric()
RMSE3<-as.numeric()
RMSE4<-as.numeric()
perctrain<-0.8

for(i in 1:5){
  my.data3rand <- my.data3[sample(nrow(my.data3)),]
  trainset <- my.data3rand[1:(nrow(my.data3rand)*perctrain),]
  testset <- my.data3rand[-(1:(nrow(my.data3rand)*perctrain)),]
  fit<-lm(BTU~.,data=trainset)
  fit<-stepAIC(fit,direction="backward",trace=0)
  #fit2<-lm(BTU~TOTSQFT+TOTHSQFT+HD65+CD65+TYPEHUQ+YEARMADE+ATHOME+HBUSNESS+NHSLDMEM+SDESCENT+MONEYPY,data=trainset)
  fit3 <- rpart(BTU~.,data=trainset,method="anova",control = rpart.control(cp = 0.003))
  numval<-predict(fit,testset)
  #numval2<-predict(fit2,testset)
  numval3<-predict(fit3,testset)
  defvec<-rep(mean(testset$BTU),nrow(testset))
  outset<-as.data.frame(cbind(testset$BTU,numval,numval3,defvec))
  names(outset)[1]<-"ACTUAL"
  SE1<-(outset$numval-outset$ACTUAL)^2
  #SE2<-(outset$numval2-outset$ACTUAL)^2
  SE3<-(outset$numval3-outset$ACTUAL)^2
  SE4<-(outset$defvec-outset$ACTUAL)^2
  RMSE1[i]<-sqrt(mean(SE1))
  #RMSE2[i]<-sqrt(mean(SE2))
  RMSE3[i]<-sqrt(mean(SE3))
  RMSE4[i]<-sqrt(mean(SE4))
}
cat("The RMSE of the full linear fit is: ",mean(RMSE1),"\n")
#cat("The RMSE of the built linear fit is: ",mean(RMSE2),"\n")
cat("The RMSE of the tree predictor is: ",mean(RMSE3),"\n")
cat("The RMSE of the default predictor is: ",mean(RMSE4),"\n")