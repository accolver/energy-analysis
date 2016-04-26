library("e1071")
library(class)
library(arules)
library(rpart)
library(ROCR)
library(party)
library(ggplot2)
options(scipen=999)

pcadata <- read.csv('pca_energy.csv')

#Pre-process PCA data into subcategories (1,0) for each output category
Output.Cat1 <- as.numeric(rep(0,length(pcadata$Output)))
Output.Cat2 <- as.numeric(rep(0,length(pcadata$Output)))
Output.Cat3 <- as.numeric(rep(0,length(pcadata$Output)))

for (b in 1:length(pcadata$Output)){
  if(pcadata$Output[b]==1){
    Output.Cat1[b] <- 1
  } else if(pcadata$Output[b]==2){
    Output.Cat2[b] <- 1
  } else {
    Output.Cat3[b] <- 1
  }
}

pcadatain <- pcadata[,-grep("Output",names(pcadata))]
pcadata.in1 <- cbind(pcadatain,Output.Cat1)
pcadata.in2 <- cbind(pcadatain,Output.Cat2)
pcadata.in3 <- cbind(pcadatain,Output.Cat3)


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
       outmod <- rpart(output~.,data=trainframe,method="class",control = rpart.control(cp = 0.001))
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
     } else if(modeltype == "nb"){
       outmod <- naiveBayes(output~.,data=trainframe)
       preddata <- predict(outmod, testframe, type="raw")
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
    perf <- performance(predframe, measure="tnr", x.measure="fnr")
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

####################
#Train Final Models#
####################

perctrain <- 0.8

#SVM
pcarand <- pcadata[sample(nrow(pcadata)),]
trainset <- pcarand[1:(nrow(pcarand)*perctrain),]
testset <- pcarand[-(1:(nrow(pcarand)*perctrain)),]
finalmod <- svm(Output~.,data=trainset)
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

#Tree
pcarand <- pcadata[sample(nrow(pcadata)),]
trainset <- pcarand[1:(nrow(pcarand)*perctrain),]
testset <- pcarand[-(1:(nrow(pcarand)*perctrain)),]
finalmod <- rpart(Output~.,data=trainset,method="class",control = rpart.control(cp = 0.01))
preddata <- predict(finalmod, testset)
preddatanum <- as.numeric()
for (pp in 1:nrow(preddata)){
  preddatanum[pp] <- which.max(as.numeric(preddata[pp,]))
}
preddatanum <- as.data.frame(preddatanum)
inframe <- data.frame(actual=testset[[ncol(testset)]],predicted=preddatanum[[1]])
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
cat('The average accuracy for the tree model is: ',accmeas,'\n')
cat('The identification rate for high values is: ',highrate,'\n')
cat('The identification rate for low values is: ',lowrate,'\n')
cat('\n')

#NB
pcarand <- pcadata[sample(nrow(pcadata)),]
trainset <- pcarand[1:(nrow(pcarand)*perctrain),]
testset <- pcarand[-(1:(nrow(pcarand)*perctrain)),]
finalmod <- naiveBayes(Output~.,data=trainset)
preddata <- predict(finalmod, testset,type="raw")
preddatanum <- as.data.frame(as.numeric(preddata))
preddatanum <- as.numeric()
for (pp in 1:nrow(preddata)){
  preddatanum[pp] <- which.max(as.numeric(preddata[pp,]))
}
preddatanum <- as.data.frame(preddatanum)
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
cat('The average accuracy for the NB model is: ',accmeas,'\n')
cat('The identification rate for high values is: ',highrate,'\n')
cat('The identification rate for low values is: ',lowrate,'\n')
cat('\n')

#Default
defvalue <- as.numeric()
vals <- c(1,2,3)
for(oo in 1:nrow(testset)){
  defvalue[oo] <- sample(vals,1,replace=TRUE)
}
inframe <- data.frame(actual=testset[[ncol(testset)]],predicted=defvalue)
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
cat('The average accuracy for the default model is: ',accmeas,'\n')
cat('The identification rate for high values is: ',highrate,'\n')
cat('The identification rate for low values is: ',lowrate,'\n')
cat('\n')

