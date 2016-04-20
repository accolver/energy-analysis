library('plyr')

import.csv <- function(filename) {
  read.csv(filename, sep = ",", header = TRUE)
}

removeColumns <- function (df, cols) {
  df[, -which(names(df) %in% cols)]
}

data.energy <- import.csv('combined_energy.csv')

colnames(data.energy)

### Info
# LP/G = Liquified Petroleum Gas

### Null indicators

# CUFEETNG 999999
# GALLON[..] 999999
# BTU[...] 999999
# ALTHUQ 9
# AGEMEM[N] 99



count(data.energy$BTUEL == 999999)

hist(data.energy$GALLONLP)

# Drop uneeded columns
redundantColumns <- c('BTUEL')
insufficientColumns <- c('GALLONKR')
data.energy <- removeColumns(data.energy, redundantColumns)
data.energy <- removeColumns(data.energy, insufficientColumns)
