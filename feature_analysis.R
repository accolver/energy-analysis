# http://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
library('caret')
library('randomForest')

data.energy <- read.csv('cleaned_data_FINAL.csv')

#Get rid of DOEID
data.energyPP <- data.energy[,-1]

#Get rid of outputs
data.energyPP <- data.energyPP[,-grep("BTU",names(data.energyPP))]
data.energyPP <- data.energyPP[,-grep("KWH",names(data.energyPP))]

# Analysis

### Recursive Feature Exclusion
numFolds <- 10
control <- rfeControl(functions = rfFuncs, method="cv", number = numFolds)

numFeatures <- length(data.energyPP)
results <- rfe(data.energyPP, data.energy$BTU, rfeControl = control, sizes = c(1:numFeatures))

print(results)

# Recursive feature selection
#
# Outer resampling method: Cross-Validated (10 fold)
#
# Resampling performance over subset size:
#
#   Variables  RMSE Rsquared RMSESD RsquaredSD Selected
# 1 49383   0.1035   4120    0.05269
# 2 44050   0.2412   1990    0.04650
# 3 40811   0.3515   1607    0.03117
# 4 39460   0.3958   1977    0.04315
# 5 38786   0.4165   1701    0.03953
# 6 38695   0.4151   1908    0.04024
# 7 38563   0.4186   1761    0.03754
# 8 38400   0.4233   1656    0.03816
# 9 38390   0.4236   1806    0.04412
# 10 38366   0.4242   1740    0.04171
# 11 38252   0.4277   1859    0.04589
# 12 38097   0.4323   1850    0.04767
# 13 38029   0.4345   1759    0.04539
# 14 38030   0.4345   1813    0.04808
# 15 38028   0.4344   1697    0.04372
# 16 37997   0.4354   1810    0.04636
# 17 37993   0.4355   1840    0.04670
# 18 37980   0.4358   1842    0.04666
# 19 38011   0.4349   1842    0.04779
# 20 37902   0.4381   1725    0.04344
# 21 37975   0.4361   1827    0.04631
# 22 37941   0.4372   1808    0.04460
# 23 37905   0.4381   1814    0.04496
# 24 37947   0.4371   1757    0.04460
# 25 37870   0.4393   1748    0.04508
# 26 37851   0.4399   1766    0.04543
# 27 37833   0.4406   1722    0.04412
# 28 37804   0.4414   1679    0.04268
# 29 37805   0.4415   1779    0.04533
# 30 37689   0.4448   1737    0.04495
# 31 37756   0.4429   1749    0.04467
# 32 37704   0.4446   1799    0.04615
# 33 37667   0.4454   1803    0.04669
# 34 37653   0.4460   1753    0.04656
# 35 37636   0.4466   1817    0.04660
# 36 37618   0.4472   1831    0.04805
# 37 37608   0.4476   1777    0.04659
# 38 37554   0.4492   1842    0.04951
# 39 37594   0.4479   1750    0.04651
# 40 37608   0.4475   1817    0.04788
# 41 37649   0.4464   1778    0.04643
# 42 37591   0.4482   1796    0.04810
# 43 37537   0.4496   1786    0.04639
# 44 37513   0.4504   1785    0.04677
# 45 37523   0.4501   1810    0.04755
# 46 37494   0.4510   1844    0.04870
# 47 37498   0.4509   1780    0.04665
# 48 37446   0.4523   1739    0.04523
# 49 37486   0.4511   1783    0.04730
# 50 37438   0.4526   1823    0.04863        *
#
#   The top 5 variables (out of 50):
#   NHSLDMEM, TYPEHUQ, HD65, TOTHSQFT, TOTSQFT

predictors(results)
# [1] "NHSLDMEM"    "TYPEHUQ"     "HD65"        "TOTHSQFT"    "TOTSQFT"     "LRGSTATE"    "AVGAGEHH"    "CD65"
# [9] "MONEYPY"     "TOTRHMSQFT"  "TOTCSQFT"    "AGEHHMEM1"   "HHAGE"       "AGEHHMEMY"   "RHMCSQFT"    "TOTUSQFT"
# [17] "TOTBASESQFT" "RHMHSQFT"    "WALLTYPE"    "TOTUCSQFT"   "UGASHERE"    "RHMUCSQFT"   "URBRUR"      "BASUCSQFT"
# [25] "YEARMADE"    "ATTUCSQFT"   "ATTUSQFT"    "TOTATTCSQFT" "TOTGARGSQFT" "GARUSQFT"    "SPOUSE"      "BASHSQFT"
# [33] "GARUCSQFT"   "RHMUSQFT"    "BASUSQFT"    "RETIREPY"    "WORKPAY"     "SDESCENT"    "OTHWORK"     "ATHOME"
# [41] "NCASHBEN"    "EMPLOYHH"    "ATTHSQFT"    "HBUSNESS"    "BASCSQFT"    "CASHBEN"     "HHSEX"       "GARHSQFT"
# [49] "ATTCSQFT"    "GARCSQFT"

plot(results, type = c("g","o"), main = "Impact of Feature Count on Error", xlab = "Variables")

num <- 6
variables <- names(unlist(results$fit$forest$xlevels))[1:num]
error <- results$results$RMSE[1:num]
plot(1:num, error, main = "Most Impactful Variables", xlab = "Variable", ylab = "Root Mean Squared Error", type = "o", xaxt = "n", col = "blue")
axis(1, at = 1:num, labels = variables)
