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


# Get device data
dev <- read.csv(file='~/analytics/pred611/data/export_Dev.csv', header=TRUE, sep=',', stringsAsFactors = FALSE)
dev <- dev[,1:3]
colnames(dev)[1] <- 'DEVICE_KEY'

##  Functions to set data up into SQL and make accessible to R 
data2SQL <- function(db_path, file_path, tableName) {
  s_time  <- proc.time()
  sqlite    <- dbDriver("SQLite")
  spirentDB <- dbConnect(sqlite,db_path)
  dbWriteTable(spirentDB, tableName, file_path)
  print("Time taken to query: ")
  print(proc.time()-s_time)
}

# Function to get one day of data
getDayData <- function(date_val, tableName) {
  s_time  <- proc.time()
  sql_str <- paste("select * from ", tableName, " where date_v = ", date_val, sep='')
  print(sql_str)
  data    <- dbGetQuery(spirentDB, sql_str)

  print("Time taken to query: ")
  print(proc.time()-s_time)
  return(data)
}

# Run once to create permanent dataset in SQL tables
fp  <- '~/analytics/pred611/data/exportKPIS_18to18Jul.csv'
tableName <- 'pred611JUL'
data2SQL(db_path = db_path, file_path = fp, tableName = tableName)

## Set up table in sqlite3 on disk for daily data over the study period
dbp <- "~/analytics/pred611/data/spirent.db"
sqlite    <- dbDriver("SQLite")
spirentDB <- dbConnect(sqlite,dbp)

##  Get target variable
fp <- '~/analytics/pred611/data/export_61108to18Jul.csv'
data2SQL(db_path = dbp, file_path = fp, tableName = 'target611JUL')
target_611 <- dbGetQuery(spirentDB, 'select * from target611JUL')
colnames(target_611)[1] <- 'CUSTOMER_KEY'
target_611$DATE_V <- substring(target_611$CALL_ANSWER_DT, 1,9)
target_611$CALL_ANSWER_DT <- NULL
colnames(target_611)[2]   <- 'DEPT_NM'

# Build dataset for periods of interest one day at a time
ds.dates <- dbGetQuery(spirentDB, 'select distinct(DATE_V) from pred611JUL')
buildDaySet <- function(day) {

  # Get one day of data from the study period
  ds <- getDayData(day, 'pred611JUL')

  # Merge target variable onto predictor dataset by customer and day columns
  system.time(ds  <- merge(x  = ds, y = target_611, by = c('CUSTOMER_KEY', 'DATE_V'), all.x = TRUE))
  table(ds$DEPT_NM)
  dim(ds)
  attach(ds)
  
  #   Variables that need recoding or alternative handling
  ds[is.na(ds[, c('P168_PCT_ATTACH_FAILURE')]), c('P168_PCT_ATTACH_FAILURE')] <- 0
  ds[is.na(ds[, c('P168_PCT_PDN_FAILURE')]), c('P168_PCT_PDN_FAILURE')]    <- 0
  ds[is.na(ds[, c('P24_PCT_ATTACH_FAILURE')]), c('P24_PCT_ATTACH_FAILURE')]  <- 0
  ds[is.na(ds[, c('P24_PCT_PDN_FAILURE')]), c('P24_PCT_PDN_FAILURE')]     <- 0
  
  # Join dummy variable dataset onto feature set
  target  <- unlist(mclapply(DEPT_NM, function(x) {return(!is.na(x))}))
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
