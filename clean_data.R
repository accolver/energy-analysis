library('plyr')

import.csv <- function(filename) {
  read.csv(filename, sep = ",", header = TRUE)
}

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

removeColumns <- function (df, cols) {
  df[, -which(names(df) %in% cols)]
}

is999999default <- function (value) {
  any(c(
    any(grep("^BTU", value)),
    any(grep("^GALLON", value)),
    any(grep("NG$", value))
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

cols <- colnames(data.energy)
threshold <- 0.5
insufficientColumns <- c()

# 9999999 defaults
colsWithDefaults <- sapply(cols, is999999default)
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
redundantColumns <- c('BTUEL')
data.energy <- removeColumns(data.energy, redundantColumns)
data.energy <- removeColumns(data.energy, insufficientColumns)

# write.csv(data.energy, 'cleaned_data.csv')
