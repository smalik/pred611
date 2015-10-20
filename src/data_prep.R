library(plyr)
library(doMC)
library(foreach)
library(doParallel)
library(parallel)
library(caret)
library(ggplot2)
library(tableplot)

# Set R to leverage 14 of the 16 cores available
doMC::registerDoMC(cores = 14)
cl <- makeCluster(14)
registerDoParallel(cl)

# Source the data in and create target variables for classification analysis
ds <- read.csv(file = "~/analytics/pred611/data/KPI_QOE_AUG.csv" , header = TRUE, sep = ',', stringsAsFactors = FALSE)
attach(ds)
table(ds, useNA = 'always')
dim(ds)

# Generate dummy matrix from 611 Call variable
tmp <- model.matrix(dummyVars( ~as.factor(DEPT_NM), data = ds))[,-c(1)]
tmp <- as.data.frame(tmp)
names(tmp) <- c('Care', 'LNP', 'Non611', 'Other', 'Tech')

# What is the mean  of 611 incidents per disposition?
laply(tmp[,c('Care', 'LNP', 'Non611', 'Other', 'Tech')], sum, .parallel = TRUE)
laply(tmp[,c('Care', 'LNP', 'Non611', 'Other', 'Tech')], mean, .parallel = TRUE)*100 # expressed as pct

# Generate new target variable from dummy matrix
targetVar_cnt     <- rep(0, dim(tmp)[1])
targetVar_binary  <- rep(0, dim(tmp)[1])
targetVar_cnt     <- parRapply(cl, tmp[, -c(3)], sum)
targetVar_binary  <- parRapply(cl, tmp[, -c(3)], max)

for (i in 1:dim(tmp)[1]) {
#   targetVar_cnt[i] <- sum(tmp[i,-c(3)])
  targetVar_cnt[i] <- max(tmp[i,-c(3)])
}
table(targetVar_cnt)
table(targetVar_binary)


# Join dummy variable dataset onto feature set
ds <- cbind(ds, tmp)
dim(ds)

# Summary statistics on columns