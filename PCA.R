library(ggplot2)

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

#Categorize output by magnitude (for classifier algorithm)
subsetBySD <- function (vect, z = 1) {
  v.sd <- sd(vect) * z
  v.mean = mean(vect)
  ifelse(vect < v.mean - v.sd, 1,
  ifelse(vect > v.mean + v.sd, 3,
        2))
}



data.energy <- read.csv('cleaned_data.csv')

#Get rid of DOEID
data.energyPP <- data.energy[,-1]

#Get rid of outputs
data.energyPP <- data.energyPP[,-grep("BTU",names(data.energyPP))]
data.energyPP <- data.energyPP[,-grep("CUFEETNG",names(data.energyPP))]
data.energyPP <- data.energyPP[,-grep("KWH",names(data.energyPP))]

#Show correlation matrix to see if PCA applicable
corrmat <- cor(data.energyPP)

#Implement PCA
set.seed(1000)
data.pca <- prcomp(data.energyPP
      , scale. = T
      , center = T
      , retx = T)

# Analyze/plot the PCA results
# plot(data.pca, type = 'l')
# screeplot(data.pca, type = "l", npcs = length(data.pca$sdev), main = "PCA Variance")
# biplot(data.pca, cex=c(0.01, 0.5))

vars <- apply(data.pca$x, 2, var)
pca.cumsum <- cumsum(vars / sum(vars))
# plot(pca.cumsum, main = "Cumulative Sum of PCA Variance", xlab = "Principle Components", ylab = "Cumulative Variance", ylim = c(0,1))

threshold <- 0.90
pca.threshold <- pca.cumsum < threshold
num.pca <- length(which(pca.threshold)) + 1
print(paste(num.pca, " components must be kept to capture ", threshold * 100, "% of the variance", sep = ""))


# Prep output variables
data.output <- data.energy$KWH # Change this to change output variable

data.output.zeros <- which(data.output == 0)
data.output[data.output.zeros] = 0.0001
data.output <- log(data.output)
data.output.cat <- subsetBySD(data.output, 0.5)


#Pass PCA data to machine learning algorithm
data.pca$x <- as.data.frame(data.pca$x)
pcadata <- cbind(data.pca$x[,1:num.pca],data.output.cat)
names(pcadata)[ncol(pcadata)] <- "Output"

#pcadata <- cbind(data.pca$x[,1:25],data.cng.cat)
#names(pcadata)[ncol(pcadata)] <- "Output"

write.csv(pcadata, 'pca_energy.csv')
