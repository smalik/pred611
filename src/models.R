# Functions to support modeling

# Generate dataset to model RF on
getModelFrame <- function(sampleN, events_ds=events, nonevents_ds=nonevents, model_Vars, id_Vars=idVars) {
  n                <- sampleN
  sample.event     <- sample(1:n) 
  sample.nonevent  <- sample(1:n)
  ind              <- sample.event
  events.1         <- cbind(events_ds[sample.event, c(model_Vars, id_Vars)], ind)
  ind              <- sample.nonevent
  nonevents.0      <- cbind(nonevents_ds[sample.nonevent, c(model_Vars, id_Vars)], ind)
  train            <- rbind(events.1, nonevents.0)
  train$ind        <- NULL
  
  print(prop.table(table(events.1$target, useNA='always')))
  print(prop.table(table(nonevents.0$target, useNA='always')))
  print(prop.table(table(train$target, useNA='always'))) # Should be a 50/50 split
  
  return(train)
}

# Run the RF model
runModel <- function(data, model_id, sizeN, mv_list, tune_param=FALSE) {
  if(tune_param){
    ntrees <- tune_param[1]
    mtry   <- tune_param[2]
  } else {
    ntrees <- 500
    mtry   <- floor(sqrt(length(mv_list)-1)/2)
  }
  
  ts <- getModelFrame(sampleN=sizeN, model_Vars=mv_list)
  if(sum(is.na(ts$target)) == 0) {
    rf <- randomForest(as.factor(ts$target) ~. , data=ts[, -c(18:20)], ntrees=ntrees, mtry=mtry, importance=TRUE, proximity=FALSE, do.trace=TRUE)  # Accuracy rate of 62%, 81% 
    print(sum(rf$confusion[c(1,4)])/sum(rf$confusion))
    return(list(rf=rf, ts=ts))
  } else {
    print('Sample chosen larger than event occurrence.  Please respecify sample frame size.')
  }
}

# Evaluate model predictions on holdout dataset
testModel <- function(model, test) {
  pred      <- predict(model, test)
  conf      <- cbind(as.logical(pred)*1, as.logical(test$target)*1)
  colnames(conf) <- c('Predicted', 'Test Set')
  conf_mat  <- table(conf[,1], conf[,2])
  conf_mat  <- confusionMatrix(conf_mat, positive= c('1'))
  return(conf_mat)
}

# computer ROC and plot it
rocModel <- function(model, test, pred_type = 'prob') {
  prob  <- predict(model, newdata=test, type= pred_type)
  if(pred_type == 'prob') {
    pred  <- prediction(prob[,2], test$target)
  } else {
    pred  <- prediction(as.logical(prob)*1, test$target)
  }
  perf  <- performance(pred, measure = 'tpr', x.measure = 'fpr')
  
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  
  roc.data <- data.frame(fpr=unlist(perf@x.values),
                         tpr=unlist(perf@y.values),
                         model= "RandomForest")
  p <- ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) + geom_ribbon(alpha=0.2) + geom_line(aes(y=tpr)) + geom_abline (intercept = 0, slope = 1)
  print(p + ggtitle(paste0("ROC Curve w/ AUC=", auc)))
}

# Generate S-Curves by response class
getSCurve <- function(rf) {
  results <- data.frame(conf=rf$votes[,1]*4+1, CALLED_611= rf$y)
  n= dim(subset(results, CALLED_611 == FALSE))[1]
  print(n)
  no    <- data.frame(pctile = (1:n)/100, Called_611= rep('No', n), qoe=sort(results$conf[results$CALLED_611 == FALSE]))
  yes   <- data.frame(pctile = (1:n)/100, Called_611= rep('Yes', n), qoe=sort(results$conf[results$CALLED_611 == TRUE]))
  df_gg <- rbind(yes, no)
  p     <- ggplot(data=df_gg, aes(x=pctile, y=qoe, group=Called_611)) + geom_line(aes(colour=Called_611)) + scale_y_continuous(breaks=seq(-1,6, by=0.5))
  print(p) 
  
  return(results)
}

# Generate Hotspots
generateHotspots <- function(rf, sampleSet) {
  results <- getSCurve(rf)
  device_qoe <- cbind(results, sampleSet[,c('CUSTOMER_KEY', 'DEVICE_KEY')])
  device_qoe$is611 <- 1*(as.logical(device_qoe$CALLED_611))
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

##  Modeling phase
for(i in ds.dates[7:11,]) {
  id      <- paste('rf_', substr(i, 4,6), substr(i,1,2), sep='')
  print(paste("Processing Model: ", id))
  ds      <- buildDaySet(paste("'",i,"'", sep=''))
  print("STEP 1 COMPLETE")
  
  allCols       <- names(ds)
  idVars        <- c('CUSTOMER_KEY', 'DEVICE_KEY')
  modelVars.PCT <- c(allCols[grepl("PCT", allCols)], 'target')
  print("STEP 2 COMPLETE")
  
  # Split dataset into test & train
  trainIndex <- createDataPartition(ds$target, p = 0.55, list = FALSE, times = 1)  # the data is too large to compute partitions on, upsample, etc...
  trainSplit  <- ds[ trainIndex,]
  events      <- subset(ds, target == 1)
  nonevents   <- subset(trainSplit, target == 0)
  rownames(events) <- 1:dim(events)[1]
  rownames(nonevents) <- 1:dim(nonevents)[1]
  train.all   <- rbind(events, nonevents[sample(1:dim(events)[1]),])
  prop.table(table(train.all$target)) # Should be a 50/50 split
  #rm(events, nonevents, ds)
  testSplit   <- ds[-trainIndex, modelVars.PCT]
  print("STEP 3 COMPLETE")
  
  if(dim(events)[1] >= 20000)
    n <- 20000
  else
    n <- dim(events)[1]
  
  model   <- runModel(model_id = id, sizeN = n, mv_list= modelVars.PCT, tune_param = c(500,5))
  rf      <- model$rf
  ts      <- model$ts
  print("STEP 4 COMPLETE")
  tmp.tm  <- testModel(rf, testSplit)
  print("STEP 5 COMPLETE")
  tmp.roc <- rocModel(rf, testSplit)  
  print("STEP 6 COMPLETE")
  
  results <- getSCurve(rf)

  # Find hotspots
  hotspot   <- generateHotspots(rf, ts)
  
  model     <- list()
  model$id  <- id
  model$day <- i
  model$rf  <- rf
  model$val <- tmp.tm
  model$roc <- tmp.roc
  model$qoe <- results
  model$devices <- ds[,c('CUSTOMER_KEY', 'DEVICE_KEY', 'target')]
  model$hotspot <- hotspot
  print("STEP 7 COMPLETE")

  # Serialize the model object
  saveRDS(model, file= paste('~/analytics/pred611/models/',id,'.RModel', sep= ''))
  rm(rf, tmp.tm, tmp.roc, model, trainSplit, trainIndex, train.all, testSplit)
  print("STEP 8 COMPLETE")
}
