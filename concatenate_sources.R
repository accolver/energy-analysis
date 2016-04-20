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

#Merge frames on FK = DOEID
combined1<-merge(x = data.energy, y = data.house, by = "DOEID", all.x = TRUE)
combined<-merge(x = combined1,y = data.household, by = "DOEID", all.x = TRUE)

write.csv(combined, 'combined_energy.csv')

