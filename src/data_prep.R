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

# Set R to leverage 14 of the 16 cores available
doMC::registerDoMC(cores = 4)
cl <- makeCluster(4)
registerDoParallel(cl)

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

# Variables from Adi's model
allCols       <- names(ds)
idVars        <- c('CUSTOMER_KEY', 'DEVICE_KEY', 'CELL_KEY')
modelVars.adi <- c('P168_PCT_PDN_FAILURE', 'P24_PCT_LTE_FAILURE', 'P24_PCT_ATTACH_FAILURE', 'P24_PCT_PDN_FAILURE', 'P168_PCT_SF_OR_FAILURE', 'P168_PCT_FAILURE', 'P168_PCT_CELL_FAILURE', 'P168_PCT_SWITCH_FAILURE', 'P168_PCT_COVERAGE_FAILURE', 'P24_PCT_SF_OR_FAILURE', 'P24_PCT_FAILURE', 'P24_PCT_CELL_FAILURE', 'P24_PCT_SWITCH_FAILURE', 'P24_PCT_COVERAGE_FAILURE', 'target')
modelVars.PCT <- c(allCols[grepl("PCT", allCols)], 'target')
  
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

testSplit   <- ds[-trainIndex[, c(1:95, 104)]

# Adi Final Model list 
# set.seed(434567)
# n = 2500
# sample.event     <- sample(1:n) 
# sample.nonevent  <- sample(1:n)
# ind <- sample.event
# events.adi    <- cbind(events[sample.event, c(modelVars.PCT, idVars)], ind)
# ind <- sample.nonevent
# nonevents.adi <- cbind(nonevents[sample.nonevent, c(modelVars.PCT, idVars)], ind)
# train.adi     <- rbind(events.adi, nonevents.adi)

##  Modeling phase

getModelFrame <- function(sampleN, events_ds=events, nonevents_ds=nonevents, model_Vars, id_Vars=idVars) {
  n <- sampleN
  sample.event     <- sample(1:n) 
  sample.nonevent  <- sample(1:n)
  ind              <- sample.event
  events.1         <- cbind(events_ds[sample.event, c(model_Vars, id_Vars)], ind)
  ind              <- sample.nonevent
  nonevents.0      <- cbind(nonevents_ds[sample.nonevent, c(model_Vars, id_Vars)], ind)
  train            <- rbind(events.1, nonevents.0)

  print(prop.table(table(events.adi$target, useNA='always')))
  print(prop.table(table(nonevents.adi$target, useNA='always')))
  print(prop.table(table(train.adi$target, useNA='always'))) # Should be a 50/50 split
  
  return(train)
}

ts <- getModelFrame(sampleN=20000, model_Vars=modelVars.PCT)

# Random Forest model following Adi's specification.  Tree size is 500.
# With 5000 samples
# Results : Accuracy ~ 
rf_adi.5k <- randomForest(as.factor(target) ~. , data=train.adi[, -c(18:21)], importance=TRUE, proximity=TRUE)  # Accuracy rate of 62%, 81% 
sum(rf_adi.5k$confusion[c(1,4)])/sum(rf_adi.5k$confusion)

# With 10k samples
rf_adi.10k <- randomForest(as.factor(target) ~. , data=train.adi, importance=TRUE, proximity=TRUE) # Accuracy rate of 76%
sum(rf_adi.10k$confusion[c(1,4)])/sum(rf_adi.10k$confusion)

# With 20k samples
rf_adi.20k <- randomForest(as.factor(target) ~. , data=ts[,-c(18:21)], importance=TRUE, proximity=TRUE) # Accuracy rate of ??
sum(rf_adi.20k$confusion[c(1,4)])/sum(rf_adi.20k$confusion)

# Oversample on training set
trainSplit$target.factor <- as.factor(trainSplit$target)
trainSplit <- SMOTE(target.factor ~ ., trainSplit, perc.over = 200, perc.under= 200)
prop.table(table(trainSplit$target))


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

# Merge device ID onto QOE result set
results <- getSCurve(rf_adi.10k)
device_qoe <- cbind(results, train.adi[,c(18:20)])
device_qoe$is611 <- as.numeric(as.character(device_qoe$CALLED_611))
device_qoe <- merge(x=device_qoe, y=dev, by='DEVICE_KEY', all.x=TRUE)
device_qoe <- ddply(device_qoe, c('MANUFACTURER', 'BRAND_MODEL'), summarise, NCalls=length(CALLED_611), mean.qoe=mean(conf), sd.qoe=sd(conf))


device_all <- ds[,c(95,104)]
device_all <- merge(x=device_all, y=dev, by='DEVICE_KEY', all.x=TRUE)
device_all <- ddply(device_all, c('MANUFACTURER', 'BRAND_MODEL'), summarise, tot.dev=NROW(target), tot.611=sum(target))
device_all$rate611 <- 100*(device_all$tot.611/device_all$tot.dev)

device_plot <- merge(x=device_qoe, y=device_all[,c('MANUFACTURER', 'BRAND_MODEL', 'rate611')], by=c('MANUFACTURER', 'BRAND_MODEL'), all.x=TRUE)

# Bubble plot of QOE by rate of 611, with total device calls as magnitude
p <- ggplot(device_plot, aes(x=mean.qoe, y=rate611)) + geom_point(aes(size=NCalls)) + scale_size_continuous(range=c(0,100)) + theme(legend.position = "top") + theme(panel.background = element_rect(colour = "pink"))
p <- ggplot(device_plot, aes(x=mean.qoe, y=rate611)) + geom_point(aes(size=NCalls)) + theme(legend.position = "top") + theme(panel.background = element_rect(colour = "pink"))
print(p)



# Take a look at counts of devices
sort(table(device_qoe$DEVICE_KEY), descending=TRUE)
sort(table(device_qoe$MANUFACTURER), decreasing=TRUE)
sort(table(device_qoe$BRAND_MODEL), decreasing=TRUE)


# Draw device hotspot plots

# Calculate rate of 611: Number of 611 calls from device/ total count of device
device_hotspot <- summaryBy(conf ~ MANUFACTURER + BRAND_MODEL, data=device_qoe, FUN=function(x) {c(avg=mean(x), tot=sum(x), cnt=NROW(x))})