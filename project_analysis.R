
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

# utility function for export to csv file
write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}


data.house <- import.csv('house.csv')
data.household <- import.csv('household.csv')
data.energy <- import.csv('energy_consumption.csv')



dim(data.house)
dim(data.household)
dim(data.energy)
