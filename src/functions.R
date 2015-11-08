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


# Generate Hotspots
generateHotspots <- function(rf, sampleSet, devTable='devices', day= DATE_V) {
  results <- getSCurve(rf)
  device_qoe <- cbind(results, sampleSet[,c('CUSTOMER_KEY', 'DEVICE_KEY')])
  device_qoe$is611 <- 1*(as.logical(device_qoe$CALLED_611))
  sql_dev_day <- paste("select CUSTOMER_KEY, DEVICE_KEY, DATE_HOUR from ", devTable, " where DATE_HOUR = '", day, "'", sep='')
  dev        <- dbGetQuery(spirentDB, sql_dev_day)
  dev$DATE_V <- substring(dev$DATE_HOUR, 1,9)
  dev$DATE_HOUR <- NULL

  device_qoe <- merge(x=device_qoe, y=dev, by='DEVICE_KEY', all.x=TRUE)
  device_qoe <- ddply(device_qoe, c('MANUFACTURER', 'BRAND_MODEL'), summarise, NCalls=length(CALLED_611), mean.qoe=mean(conf), sd.qoe=sd(conf))
  
  device_all <- ds[,c('CUSTOMER_KEY', 'DEVICE_KEY', 'target')]
  device_all <- merge(x=device_all, y=dev, by='DEVICE_KEY', all.x=TRUE)
  device_all <- ddply(device_all, c('MANUFACTURER', 'BRAND_MODEL'), summarise, tot.dev=NROW(target), tot.611=sum(target))
  device_all$rate611 <- 100*(device_all$tot.611/device_all$tot.dev)
  
  device_plot <- merge(x=device_qoe, y=device_all[,c('MANUFACTURER', 'BRAND_MODEL', 'rate611')], by=c('MANUFACTURER', 'BRAND_MODEL'), all.x=TRUE)
  # eliminate very high and very garbage device counts
  #  device_plot <- device_plot[-c(9,34,81,84:85,87,90),]
  
  # The actual plot
  p <- ggplot(subset(device_plot, NCalls > 15), aes(y=(mean.qoe), x=rate611, label=BRAND_MODEL, alpha=0.7)) + geom_point(aes(colour=NCalls, size=NCalls)) +  geom_text(vjust=1, hjust = 1, size = 5, angle=0) + scale_size_area(max_size = 50) + scale_x_continuous(name= "Rate of 611 calls per device type", limits=c(0,2)) + scale_y_continuous(name= "Average Quality of Experience per device type") + theme(legend.position = "none")+ theme(panel.background = element_rect(colour = "pink")) + theme(legend.position = 'none') + guides(fill = guide_legend(keywidth = 9, keyheight = 3)) + stat_smooth(method='lm', level=.95)
  print(p + ggtitle("Verizon Wireless QOE by device type") + theme(plot.title = element_text(size=20, face="bold")))
  
  return(device_plot)  
}