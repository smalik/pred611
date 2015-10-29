# Decks Slides
#   Show top level view:
#     1. Scatterplot of 5k & 20k model with fitted line and CI band
#     2. Accuracy of model from confusion matrix
#     3. Box plot of 0/1 class in aggregate
#     4. Boxplot of a couple devices

# S-Curve Function
getSCurve <- function(rf) {
  results <- data.frame(conf=rf$votes[,1]*4+1, CALLED_611= rf$y)
  n=nrow(results)/2
  print(n)
  no    <- data.frame(pctile = (1:n)/100, Called_611= rep('No', n), qoe=sort(results$conf[results$CALLED_611 == 0]))
  yes   <- data.frame(pctile = (1:n)/100, Called_611= rep('Yes', n), qoe=sort(results$conf[results$CALLED_611 == 1]))
  df_gg <- rbind(yes, no)
  p     <- ggplot(data=df_gg, aes(x=pctile, y=qoe, group=Called_611)) + geom_line(aes(colour=Called_611)) + scale_x_continuous(breaks=round(seq(0,100, by=5))) + scale_y_continuous(breaks=seq(-1,6, by=0.5))
  print(p + ggtitle("S-Curve discriminating between Called 611 vs. No 611 Predictions") + theme(plot.title = element_text(size=20, face="bold"))) 
  
  return(results)
}


# Scatterplot Plot function 
plotResults <- function(x, train, n) {
  results <- getSCurve(x)
  device_qoe <- cbind(results, train[,c(18:20)])
  device_qoe$is611 <- as.numeric(as.character(device_qoe$CALLED_611))
  device_qoe <- merge(x=device_qoe, y=dev, by='DEVICE_KEY', all.x=TRUE)
  device_qoe <- ddply(device_qoe, c('MANUFACTURER', 'BRAND_MODEL'), summarise, NCalls=length(CALLED_611), mean.qoe=mean(conf), sd.qoe=sd(conf))

  device_plot <- merge(x=device_qoe, y=device_all[,c('MANUFACTURER', 'BRAND_MODEL', 'rate611')], by=c('MANUFACTURER', 'BRAND_MODEL'), all.x=TRUE)
  device_plot <- device_plot[-c(9,34,81,84:85,87,90),]

  p <- ggplot(subset(device_plot, NCalls > n), aes(y=(mean.qoe), x=rate611, label=BRAND_MODEL, alpha=0.7)) + geom_point(aes(colour=NCalls, size=NCalls)) +  geom_text(vjust=1, hjust = 1, size = 5, angle=0) + scale_size_area(max_size = 50) + scale_x_continuous(name= "Rate of 611 calls per device type") + scale_y_continuous(name= "Average Quality of Experience per device type") + theme(legend.position = "none")+ theme(panel.background = element_rect(colour = "pink")) + theme(legend.position = 'none') + guides(fill = guide_legend(keywidth = 9, keyheight = 3)) + stat_smooth(method='lm', level=.95)
  print(p + ggtitle("Verizon Wireless QOE by device type") + theme(plot.title = element_text(size=20, face="bold")))
  
}

plotResults(rf_adi.5k, train.adi, 12)
plotResults(rf_adi.20k, train.adi, 50)


# Scatterplot of 5k model
# Merge device ID onto QOE result set


results.5 <- getSCurve(rf_adi.5k)
results.10 <- getSCurve(rf_adi.10k)
results.20 <- getSCurve(rf_adi.20k)
device_qoe <- cbind(results.20, train.adi[,c(18:20)])
device_qoe$is611 <- as.numeric(as.character(device_qoe$CALLED_611))
device_qoe <- merge(x=device_qoe, y=dev, by='DEVICE_KEY', all.x=TRUE)
device_qoe <- ddply(device_qoe, c('MANUFACTURER', 'BRAND_MODEL'), summarise, NCalls=length(CALLED_611), mean.qoe=mean(conf), sd.qoe=sd(conf))


device_all <- ds[,c(95,104)]
device_all <- merge(x=device_all, y=dev, by='DEVICE_KEY', all.x=TRUE)
device_all <- ddply(device_all, c('MANUFACTURER', 'BRAND_MODEL'), summarise, tot.dev=NROW(target), tot.611=sum(target))
device_all$rate611 <- 100*(device_all$tot.611/device_all$tot.dev)

device_plot <- merge(x=device_qoe, y=device_all[,c('MANUFACTURER', 'BRAND_MODEL', 'rate611')], by=c('MANUFACTURER', 'BRAND_MODEL'), all.x=TRUE)
device_plot <- device_plot[-c(9,34,81,84:85,87,90),]

# Bubble plot of QOE by rate of 611, with total device calls as magnitude
p <- ggplot(device_plot.1, aes(y=mean.qoe, x=rate611)) + geom_point(aes(size=NCalls)) + scale_size_continuous(range=c(0,100)) + scale_x_continuous(name= "Rate of 611 calls per device type", limits=c(0,2)) + scale_y_continuous(name= "Average Quality of Experience per device type", limits=c(0,10)) + theme(legend.position = "none") + theme(panel.background = element_rect(colour = "pink"))
print(p)
p + geom_abline(intercept=coef(lm(mean.qoe ~ rate611, data=device_plot.1))[1], slope=coef(lm(mean.qoe ~ rate611, data=device_plot))[2])

