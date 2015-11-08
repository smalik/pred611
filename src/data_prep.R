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
library(FactoMineR)

# Set R to leverage 14 of the 16 cores available
doMC::registerDoMC(cores = 4)
cl <- makeCluster(4)
registerDoParallel(cl)

# Read in functions from functions.R file
source('src/functions.R')

# Do you have any data to build?
runOnce <- FALSE
importDeviceData <- FALSE

## Set up table in sqlite3 on disk for daily data over the study period
# dbp <- "~/analytics/pred611/data/spirent.db"
dbp <- '~/Documents/spirent.db'
sqlite    <- dbDriver("SQLite")
spirentDB <- dbConnect(sqlite,dbp)

# Import device data into database
if(importDeviceData) {
  tableName <- 'devices'
  data2SQL(spirentDB, file_path = 'data/device_files/Part1', tableName = tableName)
  
  for( i in 2:347) {
    path <- paste('data/device_files/Part', i, sep='')
    print(path)
    print(paste('Appending file: Part', i, '...'))
    system.time(data2SQL(spirentDB, file_path = path, tableName = tableName, append_data = TRUE))
  }
  
  for( i in 1:43) {
    path <- paste('data/device_files/part347/Part', i, sep='')
    print(path)
    print(paste('Appending file: Part', i, '...'))
    system.time(data2SQL(spirentDB, file_path = path, tableName = tableName, append_data = TRUE))
  }
  cols <- names(dbGetQuery(spirentDB, 'select * from devices limit 1'))
}


if(runOnce = TRUE) {
  # Run once to create permanent dataset in SQL tables with all KPI data for study period
  fp  <- '~/analytics/pred611/data/exportKPIS_18to18Jul.csv'
  tableName <- 'pred611'
  data2SQL(dbn= spirentDB, file_path = fp, tableName = tableName)
  dbSendQuery(spirentDB, "DELETE FROM pred611 where date_v = '19-JUL-15'")
  
  fp  <- '~/analytics/pred611/data/export19julto22jul_fullkpi.csv'
  system.time(data2SQL(dbn= spirentDB, file_path = fp, tableName = tableName, append_data = TRUE))
  
  
  ##  Get target variable
  tableName <- 'target611'
  fp <- '~/analytics/pred611/data/export_61108to18Jul.csv'
  data2SQL(dbn = spirentDB, file_path = fp, tableName = tableName)
  fp <- '~/analytics/pred611/data/export611data_19to22Jul.csv'
  system.time(data2SQL(dbn = spirentDB, file_path = fp, tableName = tableName, append_data = TRUE))
}

# Clean up 
target_611 <- dbGetQuery(spirentDB, 'select * from target611')
colnames(target_611)[1] <- 'CUSTOMER_KEY'
target_611$DATE_V <- substring(target_611$CALL_ANSWER_DT, 1,9)
target_611$CALL_ANSWER_DT <- NULL
colnames(target_611)[2]   <- 'DEPT_NM'

# Build dataset for periods of interest one day at a time
ds.dates <- dbGetQuery(spirentDB, 'select distinct(DATE_V) from pred611')
buildDaySet <- function(day) {

  # Get one day of data from the study period
  ds <- getDayData(day, 'pred611')

  # Merge target variable onto predictor dataset by customer and day columns
  system.time(ds  <- merge(x  = ds, y = target_611, by = c('CUSTOMER_KEY', 'DATE_V'), all.x = TRUE))
  table(ds$DEPT_NM)
  dim(ds)

  #   Variables that need recoding or alternative handling
  ds[is.na(ds[, c('P168_PCT_ATTACH_FAILURE')]), c('P168_PCT_ATTACH_FAILURE')] <- 0
  ds[is.na(ds[, c('P168_PCT_PDN_FAILURE')]), c('P168_PCT_PDN_FAILURE')]    <- 0
  ds[is.na(ds[, c('P24_PCT_ATTACH_FAILURE')]), c('P24_PCT_ATTACH_FAILURE')]  <- 0
  ds[is.na(ds[, c('P24_PCT_PDN_FAILURE')]), c('P24_PCT_PDN_FAILURE')]     <- 0
  
  # Join dummy variable dataset onto feature set
  target  <- unlist(mclapply(ds$DEPT_NM, function(x) {return(!is.na(x))}))
  ds$DEPT_NM <- NULL
  ds$LTE_DATE_HOUR <- NULL
  ds$DATE_HOUR <- NULL
  ds    <- cbind(ds, target)

  return(ds)
}

# Split dataset into test & train
testAndTrain <- function(ds) {
  
  # Variable lists for model
  allCols       <- names(ds)
  idVars        <- c('CUSTOMER_KEY', 'DEVICE_KEY')
  modelVars.adi <- c('P168_PCT_PDN_FAILURE', 'P24_PCT_LTE_FAILURE', 'P24_PCT_ATTACH_FAILURE', 'P24_PCT_PDN_FAILURE', 'P168_PCT_SF_OR_FAILURE', 'P168_PCT_FAILURE', 'P168_PCT_CELL_FAILURE', 'P168_PCT_SWITCH_FAILURE', 'P168_PCT_COVERAGE_FAILURE', 'P24_PCT_SF_OR_FAILURE', 'P24_PCT_FAILURE', 'P24_PCT_CELL_FAILURE', 'P24_PCT_SWITCH_FAILURE', 'P24_PCT_COVERAGE_FAILURE', 'target')
  modelVars.PCT <- c(allCols[grepl("PCT", allCols)], 'target')
  modelVars.PCT_MAX <- c(allCols[grepl("PCT", allCols)], allCols[grepl("MAX", allCols)], 'target')
  
  trainIndex <- createDataPartition(ds$target, p = 0.55, list = FALSE, times = 1)  # the data is too large to compute partitions on, upsample, etc...
  trainSplit  <- ds[ trainIndex,]
  events      <- subset(ds, target == 1)
  nonevents   <- subset(trainSplit, target == 0)
  rownames(events) <- 1:dim(events)[1]
  rownames(nonevents) <- 1:dim(nonevents)[1]
  train.all   <- rbind(events, nonevents[sample(1:dim(events)[1]),])
  prop.table(table(train.all$target)) # Should be a 50/50 split
  #rm(events, nonevents, ds)
  testSplit   <- ds[-trainIndex[, modelVars.PCT]]
  rm(ds)
}
