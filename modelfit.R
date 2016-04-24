library("e1071")
library(class)
library(arules)
library(rpart)
library(ROCR)
library(party)
library(ggplot2)
options(scipen=999)

#Pre-process PCA data into subcategories (1,0) for each KWH category
KWH.Cat1 <- as.numeric(rep(0,length(pcadata$KWH.cat)))
KWH.Cat2 <- as.numeric(rep(0,length(pcadata$KWH.cat)))
KWH.Cat3 <- as.numeric(rep(0,length(pcadata$KWH.cat)))

for (b in 1:length(pcadata$KWH.cat)){
  if(pcadata$KWH.cat[b]==1){
    KWH.Cat1[b] <- 1
  } else if(pcadata$KWH.cat[b]==2){
    KWH.Cat2[b] <- 1
  } else {
    KWH.Cat3[b] <- 1
  } 
}

pcadatain <- pcadata[,-grep("KWH",names(pcadata))]
pcadata.in1 <- cbind(pcadatain,KWH.Cat1)
pcadata.in2 <- cbind(pcadatain,KWH.Cat2)
pcadata.in3 <- cbind(pcadatain,KWH.Cat3)


#Do_cv master function call performs k-fold cross validation on classifier models (requires binary output)
#Inputs: df - data frame, k - number of folds, output - output variable name
#modeltype - either "svm" or "logreg"
#Returns data frame with k, accuracy, and AUC measure for each fold; also plots ROC curve
do_cv <- function(df, k, output, modeltype) {
  outstr <- deparse(substitute(output))
  colref <- match(outstr,names(df))
  df<-cbind(df,df[[colref]])
  df<-df[,-colref]
  names(df)[ncol(df)]<-"output"
  #Randomize row entries of data frame
  set.seed(1000)
  dfrand <- df[sample(nrow(df)),]
  #Rows per fold
  numrows <- trunc(nrow(df)/k)
  startrow <- 1
  endrow <- numrows
  #Return predictions for each fold
  auc<-as.numeric()
  outframe <- data.frame(fold=0,accuracy=0,auc=0)
  for (ii in 1:k){
    testframe <- dfrand[startrow:endrow,]
    trainframe <- dfrand[-(startrow:endrow),]
    if(modeltype == "svm"){
      outmod <- svm(output~.,data=trainframe)
      preddata <- predict(outmod, testframe)
      preddatanum <- as.data.frame(as.numeric(preddata))
      preddataval <- as.numeric()
      for(kk in 1:nrow(preddatanum)){
        if(preddatanum[kk,1]>0.5){
          preddataval[kk] <- 1
        } else{
          preddataval[kk] <- 0
        }
      }
      preddataval <- as.data.frame(preddataval)
      predframe <- prediction(preddatanum[[1]],testframe[[ncol(testframe)]])
     } else if(modeltype == "tree"){
       outmod <- rpart(output~.,data=trainframe,method="class")
       preddata <- predict(outmod, testframe)
       preddatanum <- as.data.frame(preddata)
       preddataval <- as.numeric()
       for(kk in 1:nrow(preddatanum)){
         if(preddatanum[kk,1]>0.5){
           preddataval[kk] <- 0
         } else{
           preddataval[kk] <- 1
         }
       }
      preddataval <- as.data.frame(preddataval)
      predframe <- prediction(1-preddatanum[[1]],testframe[[ncol(testframe)]])
     } else {
      stop("Error - not a defined function type")
    }
    perf <- performance(predframe, measure="tpr", x.measure="fpr")
    perf2 <- performance(predframe, measure="auc")
    auc[ii] <- as.numeric(perf2@y.values)
    plot(perf, main="ROC Plot", col="blue")
    inframe <- data.frame(actual=testframe[[ncol(testframe)]],predicted=preddataval[[1]])
    acc <- 0
    for(kk in 1:nrow(inframe)){
      if(inframe[kk,1]==inframe[kk,2]){
        acc <- acc + 1
      }
    }
    accmeas <- acc/nrow(inframe)
    invec <- c(ii, accmeas, auc[ii])
    outframe <- rbind(outframe,invec)
    startrow <- startrow + numrows
    endrow <- min(nrow(dfrand),endrow + numrows)
  }
  outframe <- outframe[-1,]
  cat('The average accuracy for the k folds is: ',mean(outframe$accuracy),'\n')
  cat('The average AUC for the k folds is: ',mean(outframe$auc),'\n')
  return(outframe)
}