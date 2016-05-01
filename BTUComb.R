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

my.data <- import.csv("cleaned_data_FINAL.csv")

#inputs <- my.data[,-grep("DOEID",names(my.data))]
#inputs <- inputs[,-grep("KWH",names(inputs))]
#inputs <- inputs[,-grep("BTUEL",names(inputs))]

#predset <- subset(inputs,(inputs$BTUNG!=9999999)&(!is.na(inputs$BTUNG)))

#linfit<-lm(BTUNG~.,data=predset)
#outval <- predict(linfit,inputs)

#output <- cbind(my.data,outval)

#write.csv(output,"cleaned_data_3.csv")

#Categorize output by magnitude (for classifier algorithm)
#subsetBySD <- function (vect, z = 1) {
#  v.sd <- sd(vect) * z
#  v.mean = mean(vect)
#  ifelse(vect < v.mean - v.sd, 1,
#         ifelse(vect > v.mean + v.sd, 3,
#                2))
#}

#data.output.cat <- subsetBySD(my.data$BTU,0.5)
#my.data2 <- cbind(my.data,data.output.cat)
#my.data2 <- my.data2[,-grep("KWH",names(my.data2))]
#my.data2 <- my.data2[,-grep("BTUEL",names(my.data2))]
#my.data2 <- my.data2[,-grep("BTUNG",names(my.data2))]
#my.data2 <- my.data2[,-grep("BTU",names(my.data2))]
#names(my.data2)[ncol(my.data2)] <- "OUTPUT"

#perctrain <- 0.8

#SVM
#set.seed(1000)
#my.datarand <- my.data2[sample(nrow(my.data)),]
#trainset <- my.datarand[1:(nrow(my.datarand)*perctrain),]
#testset <- my.datarand[-(1:(nrow(my.datarand)*perctrain)),]
#finalmod <- svm(OUTPUT~.,data=trainset)
#preddata <- predict(finalmod, testset)
#preddatanum <- as.data.frame(as.numeric(preddata))
#preddatanum <- round(preddatanum[[1]],0)
#inframe <- data.frame(actual=testset[[ncol(testset)]],predicted=preddatanum)
#acc <- 0
#counthigh <- 0
#highident <- 0
#countlow <- 0
#lowident <- 0
#for(nn in 1:nrow(inframe)){
#  if(inframe[nn,1]==inframe[nn,2]){
#    acc <- acc + 1
#  }
#  if(inframe[nn,1]==3){
#    counthigh <- counthigh + 1
#  }
#  if(inframe[nn,1]==3&inframe[nn,2]==3){
#    highident <- highident + 1
#  }
#  if(inframe[nn,1]==1){
#    countlow <- countlow + 1
#  }
#  if(inframe[nn,1]==1&inframe[nn,2]==1){
#    lowident <- lowident + 1
#  }
#}
#accmeas <- acc/nrow(inframe)
#highrate <- highident/counthigh
#lowrate <- lowident/countlow
#cat('The average accuracy for the svm model is: ',accmeas,'\n')
#cat('The identification rate for high values is: ',highrate,'\n')
#cat('The identification rate for low values is: ',lowrate,'\n')
#cat('\n')

testfunc <- function(var1){
  outstr1 <- deparse(substitute(var1))
  colref1 <- match(outstr1,names(my.data3))
  fitbase <- lm(BTU~my.data[[colref1]],data=my.data3)
  fitnew1 <- lm(BTU~exp(my.data[[colref1]]),data=my.data3)
  #fitnew2 <- lm(BTU~log(my.data[[colref1]]),data=my.data3)
  fitnew3 <- lm(BTU~poly(my.data[[colref1]],3),data=my.data3)
  numval<-predict(fitbase,my.data3)
  numval2<-predict(fitnew1,my.data3)
  #numval3<-predict(fitnew2,my.data3)
  numval4<-predict(fitnew3,my.data3)
  RMSE1<-sqrt(mean((numval - my.data3$BTU)^2))
  RMSE2<-sqrt(mean((numval2 - my.data3$BTU)^2))
  #RMSE3<-sqrt(mean((numval3 - my.data3$BTU)^2))
  RMSE4<-sqrt(mean((numval4 - my.data3$BTU)^2))
  cat("RMSE 1: ",RMSE1,"\n")
  cat("RMSE 2: ",RMSE2,"\n")
  #cat("RMSE 3: ",RMSE3,"\n")
  cat("RMSE 4: ",RMSE4,"\n")
}

interact <- function(var1,var2){
  outstr1 <- deparse(substitute(var1))
  outstr2 <- deparse(substitute(var2))
  colref1 <- match(outstr1,names(my.data3))
  colref2 <- match(outstr2,names(my.data3))
  fitbase <- lm(BTU~my.data[[colref1]]+my.data[[colref2]],data=my.data3)
  fitnew <- lm(BTU~(my.data[[colref1]]:my.data[[colref2]]),data=my.data3)
  numval<-predict(fitbase,my.data3)
  numval2<-predict(fitnew,my.data3)
  RMSE1<-sqrt(mean((numval - my.data3$BTU)^2))
  RMSE2<-sqrt(mean((numval2 - my.data3$BTU)^2))
  cat("RMSE 1: ",RMSE1,"\n")
  cat("RMSE 2: ",RMSE2,"\n")
}

my.data3 <- my.data[,-grep("KWH",names(my.data))]
my.data3 <- my.data3[,-grep("BTUEL",names(my.data3))]
my.data3 <- my.data3[,-grep("BTUNG",names(my.data3))]
my.data3<-my.data3[,-grep("DOEID",names(my.data3))]

cor<-as.numeric()
for(i in 1:ncol(my.data3)){
  cor[i]<-cor(my.data3$BTU,my.data3[[i]])
}

RMSE1<-as.numeric()
RMSE2<-as.numeric()
RMSE3<-as.numeric()
RMSE4<-as.numeric()
perctrain<-0.8

set.seed(1000)
for(i in 1:1){
my.data3rand <- my.data3[sample(nrow(my.data3)),]
trainset <- my.data3rand[1:(nrow(my.data3rand)*perctrain),]
testset <- my.data3rand[-(1:(nrow(my.data3rand)*perctrain)),]
fit<-lm(BTU~.+poly(TOTSQFT,3)+poly(TOTHSQFT,3)+poly(TOTRHMSQFT,3)+poly(TOTCSQFT,3)+poly(TOTUCSQFT,3)+poly(HD65,3)+poly(AVGAGEHH,3),data=trainset)
fit<-stepAIC(fit,direction="backward",trace=0)
fit2<-lm(BTU~.,data=trainset)
fit2<-stepAIC(fit2,direction="backward",trace=0)
#fit2<-lm(BTU~TOTSQFT+TOTHSQFT+HD65+CD65+TYPEHUQ+YEARMADE+ATHOME+HBUSNESS+NHSLDMEM+SDESCENT+MONEYPY,data=trainset)
fit3 <- rpart(BTU~.,data=trainset,method="anova",control = rpart.control(cp = 0.003))
numval<-predict(fit,testset)
numval2<-predict(fit2,testset)
numval3<-predict(fit3,testset)
defvec<-rep(mean(testset$BTU),nrow(testset))
outset<-as.data.frame(cbind(testset$BTU,numval,numval2,numval3,defvec))
names(outset)[1]<-"ACTUAL"
SE1<-(outset$numval-outset$ACTUAL)^2
SE2<-(outset$numval2-outset$ACTUAL)^2
SE3<-(outset$numval3-outset$ACTUAL)^2
SE4<-(outset$defvec-outset$ACTUAL)^2
RMSE1[i]<-sqrt(mean(SE1))
RMSE2[i]<-sqrt(mean(SE2))
RMSE3[i]<-sqrt(mean(SE3))
RMSE4[i]<-sqrt(mean(SE4))
}
cat("The RMSE of the quadratic fit is: ",mean(RMSE1),"\n")
cat("The RMSE of the linear fit is: ",mean(RMSE2),"\n")
cat("The RMSE of the tree predictor is: ",mean(RMSE3),"\n")
cat("The RMSE of the default predictor is: ",mean(RMSE4),"\n")