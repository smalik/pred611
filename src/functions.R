# Get device data
getDeviceList <- function(path= '~/analytics/pred611/data/export_Dev.csv') {
  dev <- read.csv(file=path, header=TRUE, sep=',', stringsAsFactors = FALSE)
  dev <- dev[,1:3]
  colnames(dev)[1] <- 'DEVICE_KEY'

  return(dev)
}

##  Functions to set data up into SQL and make accessible to R 
getTableFields <- function(dbn, table) {
  sql_str <- paste('select * from ', table, ' limit 1', sep='')
  fields  <- dbGetQuery(dbn, sql_str)
  fields  <- noquote(sort(names(fields)))
  fields  <- paste(fields, collapse= ', ')
  return(fields)
}

data2SQL <- function(dbn, file_path, tableName, append_data = FALSE) {
  s_time  <- proc.time()
  if(append_data == FALSE) {
    dbWriteTable(dbn, tableName, file_path, append=append_data)
  } else if(append_data == TRUE) {
    # Steps:
      # load data into a tmp table
      if('tmp' %in% dbListTables(spirentDB)) {
        dbRemoveTable(dbn, 'tmp')
      }
      dbWriteTable(dbn, 'tmp', file_path, overwrite=TRUE)

      fields.tn   <- sort(getTableFields(dbn, tableName))
      fields.tmp  <- sort(getTableFields(dbn, 'tmp'))
#       print(fields.tn) 
#       print(fields.tmp)
      if(fields.tn == fields.tmp) {
        sql_str <- paste('INSERT INTO ', tableName, ' (', fields.tn,') select ', fields.tmp, ' from tmp', sep= '')
        dbSendQuery(dbn, sql_str)
      } else if(fields.tn != fields.tmp) {
        print("There is a mismatch in column names of the two datasets you are trying to stack.")
        print(fields.tn == fields.tmp)
      }
  }
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
