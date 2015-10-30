# Functions to support modeling

# Generate dataset to model RF on
getModelFrame <- function(sampleN, events_ds=events, nonevents_ds=nonevents, model_Vars, id_Vars=idVars) {
  n <- sampleN
  sample.event     <- sample(1:n) 
  sample.nonevent  <- sample(1:n)
  ind              <- sample.event
  events.1         <- cbind(events_ds[sample.event, c(model_Vars, id_Vars)], ind)
  ind              <- sample.nonevent
  nonevents.0      <- cbind(nonevents_ds[sample.nonevent, c(model_Vars, id_Vars)], ind)
  train            <- rbind(events.1, nonevents.0)
  
  print(prop.table(table(events.1$target, useNA='always')))
  print(prop.table(table(nonevents.0$target, useNA='always')))
  print(prop.table(table(train$target, useNA='always'))) # Should be a 50/50 split
  
  return(train)
}

# Generate S-Curves by response class
getSCurve <- function(rf) {
  results <- data.frame(conf=rf$votes[,1]*4+1, CALLED_611= rf$y)
  n=nrow(results)/2
  print(n)
  no    <- data.frame(pctile = (1:n)/100, Called_611= rep('No', n), qoe=sort(results$conf[results$CALLED_611 == 0]))
  yes   <- data.frame(pctile = (1:n)/100, Called_611= rep('Yes', n), qoe=sort(results$conf[results$CALLED_611 == 1]))
  df_gg <- rbind(yes, no)
  p     <- ggplot(data=df_gg, aes(x=pctile, y=qoe, group=Called_611)) + geom_line(aes(colour=Called_611)) + scale_x_continuous(breaks=round(seq(0,100, by=5))) + scale_y_continuous(breaks=seq(-1,6, by=0.5))
  print(p) 
  
  return(results)
}


# Variables from Adi's model
allCols       <- names(ds)
idVars        <- c('CUSTOMER_KEY', 'DEVICE_KEY')
modelVars.adi <- c('P168_PCT_PDN_FAILURE', 'P24_PCT_LTE_FAILURE', 'P24_PCT_ATTACH_FAILURE', 'P24_PCT_PDN_FAILURE', 'P168_PCT_SF_OR_FAILURE', 'P168_PCT_FAILURE', 'P168_PCT_CELL_FAILURE', 'P168_PCT_SWITCH_FAILURE', 'P168_PCT_COVERAGE_FAILURE', 'P24_PCT_SF_OR_FAILURE', 'P24_PCT_FAILURE', 'P24_PCT_CELL_FAILURE', 'P24_PCT_SWITCH_FAILURE', 'P24_PCT_COVERAGE_FAILURE', 'target')
modelVars.PCT <- c(allCols[grepl("PCT", allCols)], 'target')
modelVars.PCT_MAX <- c(allCols[grepl("PCT", allCols)], allCols[grepl("MAX", allCols)], 'target')

# Split dataset into test & train
set.seed(3456)
trainIndex <- createDataPartition(ds$target, p = 0.55, list = FALSE, times = 1)  # the data is too large to compute partitions on, upsample, etc...
head(trainIndex)

trainSplit  <- ds[ trainIndex,]
events      <- subset(ds, target == 1)
nonevents   <- subset(trainSplit, target == 0)
rownames(events) <- 1:dim(events)[1]
rownames(nonevents) <- 1:dim(nonevents)[1]
train.all   <- rbind(events, nonevents[sample(1:dim(events)[1]),])
prop.table(table(train.all$target)) # Should be a 50/50 split

rm(events, nonevents, ds)

testSplit   <- ds[-trainIndex[, modelVars.PCT]]
                  

##  Modeling phase


ts <- getModelFrame(sampleN=10000, model_Vars=modelVars.PCT)

# Random Forest model following Adi's specification.  Tree size is 500.
# With 5000 samples
# Results : Accuracy ~ 
rf_0715.5k <- randomForest(as.factor(target) ~. , data=ts[, -c(18:20)], importance=TRUE, proximity=TRUE)  # Accuracy rate of 62%, 81% 
sum(rf_0715.5k$confusion[c(1,4)])/sum(rf_0715.5k$confusion)
saveRDS(rf_0715.5k, file = "~/analytics/pred611/models/rf_0715.5k.RModel", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)

rf_0715.5ak <- randomForest(as.factor(target) ~. , data=ts[, -c(18:20)], ntrees=1000, mtry=4, importance=TRUE, proximity=TRUE)  # Accuracy rate of 62%, 81% 
sum(rf_0715.5ak$confusion[c(1,4)])/sum(rf_0715.5ak$confusion)
saveRDS(rf_0715.5ak, file = "~/analytics/pred611/models/rf_0715.5ak.RModel", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)

# With 10k samples
rf_0715.10k <- randomForest(as.factor(target) ~. , data=ts[,-c(18:20)], importance=TRUE, proximity=TRUE) # Accuracy rate of 76%
sum(rf_0715.10k$confusion[c(1,4)])/sum(rf_0715.10k$confusion)
saveRDS(rf_0715.10k, file = "~/analytics/pred611/models/rf_0715.10k.RModel", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)

# With 20k samples
rf_adi.20k <- randomForest(as.factor(target) ~. , data=ts[,-c(18:21)], importance=TRUE, proximity=TRUE) # Accuracy rate of ??
sum(rf_adi.20k$confusion[c(1,4)])/sum(rf_adi.20k$confusion)
saveRDS(rf_adi.20k, file = "~/analytics/pred611/models/rf_adi.20k.RModel", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)

# Oversample on training set
trainSplit$target.factor <- as.factor(trainSplit$target)
trainSplit <- SMOTE(target.factor ~ ., trainSplit, perc.over = 200, perc.under= 200)
prop.table(table(trainSplit$target))


