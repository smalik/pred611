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
  sql_str <- paste("select * from", tableName, "where date_v =", date_val)
  data    <- dbGetQuery(spirentDB, sql_str)

  print("Time taken to query: ")
  print(proc.time()-s_time)
  return(data)
}

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

## Set up table in sqlite3 on disk for daily data over the study period
dbp <- "~/analytics/pred611/data/spirent.db"
fp  <- '~/analytics/pred611/data/exportKPIS_18to18Jul.csv'
data2SQL(db_path = dbp, file_path = fp, tableName = 'pred611JUL')

##  Get target variable
fp <- '~/analytics/pred611/data/export_61108to18Jul.csv'
data2SQL(db_path = dbp, file_path = fp, tableName = 'target611JUL')
target_611 <- dbGetQuery(spirentDB, 'select * from target611JUL')
colnames(target_611)[1] <- 'CUSTOMER_KEY'
target_611$DATE_V <- substring(target_611$CALL_ANSWER_DT, 1,9)
target_611$CALL_ANSWER_DT <- NULL
colnames(target_611)[2]   <- 'DEPT_NM'

# Get one day of data from the study period
rm(ds)
date_v <- "'15-JUL-15'"
ds <- getDayData(date_v, 'pred611JUL')

# Merge target variable onto predictor dataset by customer and day columns
system.time(ds  <- merge(x  = ds, y = target_611, by = c('CUSTOMER_KEY', 'DATE_V'), all.x = TRUE))
table(ds$DEPT_NM)
dim(ds)
rm(target_611)
attach(ds)


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
target  <- unlist(mclapply(DEPT_NM, function(x) {return(!is.na(x))}))
ds$DEPT_NM <- NULL
ds$LTE_DATE_HOUR <- NULL
ds$DATE_HOUR <- NULL
ds    <- cbind(ds, target)
dim(ds)
names(ds)

# Transformations
