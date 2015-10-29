library(plyr)
library(doMC)
library(foreach)
library(doParallel)
library(parallel)
library(caret)
library(ggplot2)
library(tableplot)
library(DMwR)
library(unbalanced)
library(ParallelForest)
library(penalized)
library(glmnet)
library(randomForest)
library(ROCR)
library(sqldf)
library(RSQLite)

# Set R to leverage 14 of the 16 cores available
doMC::registerDoMC(cores = 4)
cl <- makeCluster(4)
registerDoParallel(cl)

### For initial August 1, 2015 slice of data
# Get device data
dev <- read.csv(file='~/analytics/pred611/data/export_Dev.csv', header=TRUE, sep=',', stringsAsFactors = FALSE)
dev <- dev[,1:3]
colnames(dev)[1] <- 'DEVICE_KEY'

# Source the data in and create target variables for classification analysis
ds <- read.csv(file = "~/analytics/pred611/data/KPI_QOE_AUG.csv" , header = TRUE, sep = ',', stringsAsFactors = FALSE)
nrows <- dim(ds)[1]
ncols <- dim(ds)[2]-5
attach(ds)
table(ds, useNA = 'always')
dim(ds)

### Import data from CSV file for entire study period
# Setup spirent database as SQLite
sqlite    <- dbDriver("SQLite")
spirentDB <- dbConnect(sqlite,"~/analytics/pred611/data/spirent.db")

fpath = '~/analytics/pred611/data/exportKPIS_18to18Jul.csv'
dbWriteTable(spirentDB, 'pred611JUL', fpath)
dbGetQuery(spirentDB, "select count(*) from pred611JUL where date_v = '09-JUL-15' group by ")

### Set up table in sqlite3 on disk for daily data over the study period (Aug 2015)
fpath = '~/analytics/pred611/data/exportKPIS_18to18Jul.csv'
ds <- read.csv.sql(fpath , sql= "INSERT INTO pred611 select * from file" , dbname= "spirent.pred611", drv= 'SQLite') 

# Generate dummy matrix from 611 Call variable
tmp <- model.matrix(dummyVars( ~as.factor(DEPT_NM), data = ds))[,-c(1)]
tmp <- as.data.frame(tmp)
names(tmp) <- c('Care', 'LNP', 'Non611', 'Other', 'Tech')

# What is the mean  of 611 incidents per disposition?
laply(tmp[,c('Care', 'LNP', 'Non611', 'Other', 'Tech')], sum, .parallel = TRUE) # (56422      200 11490568      352    11650)
laply(tmp[,c('Care', 'LNP', 'Non611', 'Other', 'Tech')], mean, .parallel = TRUE)*100 # expressed as pct::  (0.48799094  0.00172979 99.38132346  0.00304443  0.10076024)

# Generate new target variable from dummy matrix
targetVar_cnt     <- rep(0, nrows)
targetVar_binary  <- rep(0, nrows)

# Variable for count data modeling
targetVar_cnt     <- parRapply(cl, tmp[, -c(3)], sum)

# Variable for dichotomous response model
targetVar_binary  <- parRapply(cl, tmp[, -c(3)], max)

# Check counts
table(targetVar_cnt)
table(targetVar_binary)

# Counts dont line up...why?
#     targetVar_binary
#     0           1 
#     11493476    68624 
#     
#     targetVar_cnt   //Not useful.  No counts
#     0           1 
#     11493476    68624 

# Checking for why:
foreach(1:ncols) %dopar% { x<- sum(tmp[i,]); if(x>1) print(i)}  # All rows sum to 1.  No mistake here
foreach(i:length(targetVar_cnt)) %dopar% {if(targetVar_binary[i]>1) print(i)} # All rows are 0/1.  No mistake here

# Lets plot new target variables
boxplot(targetVar_binary)


# Summary statistics and density plots on columns w.r.t target variable
foreach(i = 1:2) %do% {
  print(paste0(i, "- Variable name: ", names(ds)[i]))
  print("------------------------------------------------------")
  s <- summary(ds[,i])
  print(s)
  print("------------------------------------------------------")
  if (s[6] > 1) {
    dat <- data.frame(col1= ds[,i], col2=targetVar_binary)
    ggplot(dat, aes(x=dat[,1])) + geom_density(aes(group=targetVar_binary, colour=targetVar_binary, fill=targetVar_binary), alpha=0.3) 
  }
}


#   Variables that need recoding or alternative handling
# 
#     P168_PCT_ATTACH_FAILURE
#     P168_PCT_PDN_FAILURE
#     P24_PCT_ATTACH_FAILURE
#     P24_PCT_PDN_FAILURE
ds[is.na(ds[, c('P168_PCT_ATTACH_FAILURE')]), c('P168_PCT_ATTACH_FAILURE')] <- 0
ds[is.na(ds[, c('P168_PCT_PDN_FAILURE')]), c('P168_PCT_PDN_FAILURE')]    <- 0
ds[is.na(ds[, c('P24_PCT_ATTACH_FAILURE')]), c('P24_PCT_ATTACH_FAILURE')]  <- 0
ds[is.na(ds[, c('P24_PCT_PDN_FAILURE')]), c('P24_PCT_PDN_FAILURE')]     <- 0

# Join dummy variable dataset onto feature set
target  <- targetVar_binary
ds    <- cbind(ds, tmp, target)
dim(ds)
names(ds)
rm(dat, tmp)


# Transformations
