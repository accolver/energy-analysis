library('plyr')

import.csv <- function(filename) {
  read.csv(filename, sep = ",", header = TRUE)
}

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

# Remove columns (cols) from a data frame (df)
removeColumns <- function (df, cols) {
  df[, -which(names(df) %in% cols)]
}

# Replace all fields in `data` matching `value` with sample fields from `data`
replaceWithSampling <- function (value, data) {
  nc <- ncol(data)
  for (i in 1:nc) {
    col <- data[, i]
    sampleData <- col[which(col != value || !is.na(col))]
    for (j in 1:length(col)) {
      if (is.na(col[j]) || col[j] == value) {
        data[, i][j] <- sample(sampleData, 1)
      }
    }
  }
  data
}

is9999999default <- function (value) {
  any(c(
    any(grep("^BTU", value))
  ))
}

is999999default <- function (value) {
  any(c(
    any(grep("^GALLON", value)),
    any(grep("^CU", value))
  ))
}

is99default <- function (value) {
  any(
    grep("^AGEHHMEM", value)
  )
}

is9default <- function (value) {
  any(
    grep("ALTHUQ", value)
  )
}

removeCol <- function(data,default,thresh){
  count <- 0
  numdata <- as.numeric(data)
  for(i in 1:length(numdata)){
    if ((numdata[i]==default)||is.na(numdata[i])){
      count <- count + 1
    }
  }
  percdefault <- count/length(numdata)
  percdefault > thresh
}


###################
# PROCESS THE DATA
###################

data.energy <- import.csv('combined_energy.csv')

#Add new field = average age of household member
agemat <- data.energy[,grep("AGEHH",names(data.energy))]
avgage <- as.numeric()
for (a in 1:nrow(agemat)){
  vec1 <- as.numeric(agemat[a,])
  avgage[a] <- mean(vec1[vec1 != 99])
}
data.energy <- cbind(data.energy,avgage)
names(data.energy)[ncol(data.energy)] <- "AVGAGEHH"

cols <- colnames(data.energy)
threshold <- 0.5
insufficientColumns <- c()

# 9999999 defaults
colsWithDefaults <- sapply(cols, is9999999default)
toRemove <- sapply(data.energy[colsWithDefaults], function (x) {
  removeCol(x, 9999999, threshold)
})
toRemove <- names(toRemove[which(toRemove)])
insufficientColumns <- c(insufficientColumns, toRemove)

# 999999 defaults
colsWithDefaults <- sapply(cols, is999999default)
toRemove <- sapply(data.energy[colsWithDefaults], function (x) {
  removeCol(x, 999999, threshold)
})
toRemove <- names(toRemove[which(toRemove)])
insufficientColumns <- c(insufficientColumns, toRemove)

# 99 defaults
colsWithDefaults <- sapply(cols, is99default)
toRemove <- sapply(data.energy[colsWithDefaults], function (x) {
  removeCol(x, 99, threshold)
})
toRemove <- names(toRemove[which(toRemove)])
insufficientColumns <- c(insufficientColumns, toRemove)

# 9 defaults
colsWithDefaults <- sapply(cols, is9default)
toRemove <- sapply(data.energy[colsWithDefaults], function (x) {
  removeCol(x, 9, threshold)
})
toRemove <- names(toRemove[which(toRemove)])
insufficientColumns <- c(insufficientColumns, toRemove)


# Drop uneeded columns
redundantColumns <- c('BTUEL','BTUNG')
data.energy <- removeColumns(data.energy, redundantColumns)
data.energy <- removeColumns(data.energy, insufficientColumns)


################
# FILL THE DATA
################


set.seed(333) # Reproducible results

#Sample 9999999
cols <- colnames(data.energy)
colsWithDefaults <- sapply(cols, is9999999default)
if (any(colsWithDefaults)) {
  sub.data.energy <- data.energy[colsWithDefaults]
  sub.data.energy <- replaceWithSampling(9999999, sub.data.energy)
  data.energy <- data.energy[!colsWithDefaults]
  data.energy <- cbind(data.energy, sub.data.energy)
}

#Sample 999999
cols <- colnames(data.energy)
colsWithDefaults <- sapply(cols, is999999default)
if (any(colsWithDefaults)) {
  sub.data.energy <- data.energy[colsWithDefaults]
  sub.data.energy <- replaceWithSampling(999999, sub.data.energy)
  data.energy <- data.energy[!colsWithDefaults]
 data.energy <- cbind(data.energy, sub.data.energy)
}

#Sample 99
cols <- colnames(data.energy)
colsWithDefaults <- sapply(cols, is99default)
if (any(colsWithDefaults)) {
  sub.data.energy <- data.energy[colsWithDefaults]
  sub.data.energy <- replaceWithSampling(99, sub.data.energy)
  data.energy <- data.energy[!colsWithDefaults]
  data.energy <- cbind(data.energy, sub.data.energy)
}

#Sample 9
cols <- colnames(data.energy)
colsWithDefaults <- sapply(cols, is9default)
if (any(colsWithDefaults)) {
  sub.data.energy <- data.energy[colsWithDefaults]
  sub.data.energy <- replaceWithSampling(9, sub.data.energy)
  data.energy <- data.energy[!colsWithDefaults]
  data.energy <- cbind(data.energy, sub.data.energy)
}

# Write out the file
write.csv(data.energy, 'cleaned_data.csv')
