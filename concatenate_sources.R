import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

# Import raw data
data.energy <- import.csv('energy_consumption.csv')
data.house <- import.csv('house.csv')
data.household <- import.csv('household.csv')

# All data's rows correspond
# Drop redundant Primray Key
data.house <- data.house[, -1]
data.household <- data.household[, -1]

# Concatenate data
combined <- cbind(
  data.energy,
  data.house,
  data.household
)

write.csv(combined, 'combined_energy.csv')
