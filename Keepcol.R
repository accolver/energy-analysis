
my.data<-read.csv("combined_energy.csv")

keepvec<-as.numeric()

#Need to pass vector of default data into keepcol
for(j in 1:ncol(my.data)){
  keepvec[j]<-keepcol(my.data[,j],999999,0.5)
}

keepcol <- function(data,default,thresh){
  count <- 0
  numdata <- as.numeric(data)
  for(i in 1:length(numdata)){
    if ((numdata[i]==default)||is.na(numdata[i])){
      count <- count + 1
    } 
  }
  percdefault <- count/length(numdata)
  if(percdefault>thresh){
    return(0)
  } else{
    return(1)
  }
}

namevec<-as.character()
for(k in 1:length(keepvec)){
  if(keepvec[k]==1){
    namevec <- c(namevec,names(my.data[k]))
  }
}

my.data.filt <- my.data[,namevec]