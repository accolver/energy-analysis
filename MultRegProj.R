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



my.data <- import.csv("cleaned_data.csv")
my.data <- my.data[,-grep(("DOEID"),names(my.data))]
my.data <- my.data[,-grep(("BTU"),names(my.data))]
my.data <- my.data[,-grep(("CUFEET"),names(my.data))]


fit<-lm(KWH~.,data=my.data)
step<-stepAIC(fit,direction="backward")
numval<-predict(fit,my.data)
defvec<-rep(mean(my.data$KWH),nrow(my.data))
outset<-as.data.frame(cbind(my.data$KWH,numval,defvec))
names(outset)[1]<-"ACTUAL"
SE1<-(outset$numval-outset$ACTUAL)^2
SE2<-(outset$defvec-outset$ACTUAL)^2
RMSE1<-sqrt(mean(SE1))
RMSE2<-sqrt(mean(SE2))
print(fit$coefficients)
cat("The RMSE of the lienar fit is: ",RMSE1,"\n")
cat("The RMSE of the default predictor is: ",RMSE2,"\n")


#print(step$anova)

#Categorize output by magnitude (for classifier algorithm)
#subsetBySD <- function (vect, z = 1) {
#  v.sd <- sd(vect) * z
#  v.mean = mean(vect)
#  ifelse(vect < v.mean - v.sd, 1,
#         ifelse(vect > v.mean + v.sd, 3,
#                2))
#}

#data.output.cat <- subsetBySD(my.data$KWH,0.67)
#my.data <- cbind(my.data,data.output.cat)
#my.data <- my.data[,-grep("KWH",names(my.data))]
#names(my.data)[ncol(my.data)] <- "OUTPUT"

#perctrain <- 0.8

#SVM
#set.seed(1000)
#my.datarand <- my.data[sample(nrow(my.data)),]
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

