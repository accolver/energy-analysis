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
subsetBySD <- function (vect, z = 1) {
  v.sd <- sd(vect) * z
  v.mean = mean(vect)
  ifelse(vect < v.mean - v.sd, 1,
         ifelse(vect > v.mean + v.sd, 3,
                2))
}

data.output.cat <- subsetBySD(my.data$BTU,0.5)
my.data2 <- cbind(my.data,data.output.cat)
my.data2 <- my.data2[,-grep("KWH",names(my.data2))]
my.data2 <- my.data2[,-grep("BTUEL",names(my.data2))]
my.data2 <- my.data2[,-grep("BTUNG",names(my.data2))]
my.data2 <- my.data2[,-grep("BTU",names(my.data2))]
names(my.data2)[ncol(my.data2)] <- "OUTPUT"

#perctrain <- 0.8

#SVM
set.seed(1000)
my.datarand <- my.data2[sample(nrow(my.data)),]
trainset <- my.datarand[1:(nrow(my.datarand)*perctrain),]
testset <- my.datarand[-(1:(nrow(my.datarand)*perctrain)),]
finalmod <- svm(OUTPUT~.,data=trainset)
preddata <- predict(finalmod, testset)
preddatanum <- as.data.frame(as.numeric(preddata))
preddatanum <- round(preddatanum[[1]],0)
inframe <- data.frame(actual=testset[[ncol(testset)]],predicted=preddatanum)
acc <- 0
counthigh <- 0
highident <- 0
countlow <- 0
lowident <- 0
for(nn in 1:nrow(inframe)){
  if(inframe[nn,1]==inframe[nn,2]){
    acc <- acc + 1
  }
  if(inframe[nn,1]==3){
    counthigh <- counthigh + 1
  }
  if(inframe[nn,1]==3&inframe[nn,2]==3){
    highident <- highident + 1
  }
  if(inframe[nn,1]==1){
    countlow <- countlow + 1
  }
  if(inframe[nn,1]==1&inframe[nn,2]==1){
    lowident <- lowident + 1
  }
}
accmeas <- acc/nrow(inframe)
highrate <- highident/counthigh
lowrate <- lowident/countlow
cat('The average accuracy for the svm model is: ',accmeas,'\n')
cat('The identification rate for high values is: ',highrate,'\n')
cat('The identification rate for low values is: ',lowrate,'\n')
cat('\n')


my.data3 <- my.data[,-grep("KWH",names(my.data))]
my.data3 <- my.data3[,-grep("BTUEL",names(my.data3))]
my.data3 <- my.data3[,-grep("BTUNG",names(my.data3))]
fit<-lm(BTU~.,data=my.data3)
fit<-stepAIC(fit,direction="backward")
numval<-predict(fit,my.data3)
defvec<-rep(mean(my.data3$BTU),nrow(my.data3))
outset<-as.data.frame(cbind(my.data3$BTU,numval,defvec))
names(outset)[1]<-"ACTUAL"
SE1<-(outset$numval-outset$ACTUAL)^2
SE2<-(outset$defvec-outset$ACTUAL)^2
RMSE1<-sqrt(mean(SE1))
RMSE2<-sqrt(mean(SE2))
cat("The RMSE of the lienar fit is: ",RMSE1,"\n")
cat("The RMSE of the default predictor is: ",RMSE2,"\n")