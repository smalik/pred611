
# Merge device ID onto QOE result set
results.5 <- getSCurve(rf_adi.5k)
results.10 <- getSCurve(rf_adi.10k)
results.20 <- getSCurve(rf_adi.20k)
device_qoe <- cbind(results, ts[,c(18:20)])
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


device_plot$radius <-sqrt(device_plot$NCalls/pi)
p <- ggplot(subset(device_plot, NCalls > 15), aes(y=mean.qoe, x=rate611, label=BRAND_MODEL, alpha=0.7)) + geom_point(aes(colour=NCalls, size=NCalls)) +  geom_text(vjust=1, hjust = 1, size = 5, angle=0) + scale_size_area(max_size = 50) + scale_x_continuous(name= "Rate of 611 calls per device type", limits=c(-0.5,1.5)) + scale_y_continuous(name= "Average Quality of Experience per device type", limits=c(2.75,4)) + theme(legend.position = "none")+ theme(panel.background = element_rect(colour = "pink")) + theme(legend.position = 'none') + guides(fill = guide_legend(keywidth = 9, keyheight = 3)) + stat_smooth()
print(p + ggtitle("Verizon Wireless QOE by device type") + theme(plot.title = element_text(size=20, face="bold")))
p1 <- ggplot(subset(device_plot, NCalls > 15), aes(y=mean.qoe, x=rate611, label=BRAND_MODEL, alpha=0.7)) + geom_point(aes(colour=NCalls, size=NCalls)) +  geom_text(vjust=1, hjust = 1, size = 5, angle=0) + scale_size_area(max_size = 50) + scale_x_continuous(name= "Rate of 611 calls per device type", limits=c(-0.5,8)) + scale_y_continuous(name= "Average Quality of Experience per device type", limits=c(2,4.5)) + theme(legend.position = "none")+ theme(panel.background = element_rect(colour = "pink")) + theme(legend.position = 'none') + guides(fill = guide_legend(keywidth = 9, keyheight = 3)) + stat_smooth()
print(p1 + ggtitle("Verizon Wireless QOE by device type") + theme(plot.title = element_text(size=20, face="bold")))
p + geom_abline(intercept=coef(lm(mean.qoe ~ rate611, data=device_plot))[1], slope=coef(lm(mean.qoe ~ rate611, data=device_plot))[2])

p <- ggplot(subset(device_plot, NCalls > 15), aes(y=(mean.qoe), x=rate611, label=BRAND_MODEL, alpha=0.7)) + geom_point(aes(colour=NCalls, size=NCalls)) +  geom_text(vjust=1, hjust = 1, size = 5, angle=0) + scale_size_area(max_size = 50) + scale_x_continuous(name= "Rate of 611 calls per device type", limits=c(0,2)) + scale_y_continuous(name= "Average Quality of Experience per device type") + theme(legend.position = "none")+ theme(panel.background = element_rect(colour = "pink")) + theme(legend.position = 'none') + guides(fill = guide_legend(keywidth = 9, keyheight = 3)) + stat_smooth(method='lm', level=.95)
print(p + ggtitle("Verizon Wireless QOE by device type") + theme(plot.title = element_text(size=20, face="bold")))


# Take a look at counts of devices
sort(table(device_qoe$DEVICE_KEY), descending=TRUE)
sort(table(device_qoe$MANUFACTURER), decreasing=TRUE)
sort(table(device_qoe$BRAND_MODEL), decreasing=TRUE)


# Draw device hotspot plots

# Calculate rate of 611: Number of 611 calls from device/ total count of device
device_hotspot <- summaryBy(conf ~ MANUFACTURER + BRAND_MODEL, data=device_qoe, FUN=function(x) {c(avg=mean(x), tot=sum(x), cnt=NROW(x))})



# K-means plots
df <- subset(device_plot, NCalls > 15)
cl <- kmeans(df[,c(4:6)], 6)
print(summary(cl))
plot(cl)

x <- df
x$cluster <- factor(cl$cluster)
centers <- as.data.frame(cl$centers)
p <- ggplot(data=x, aes(x = rate611, y= mean.qoe, color=cluster), guide=FALSE) + geom_point(aes(label=BRAND_MODEL)) + geom_point(data=centers, aes(x=rate611,y=mean.qoe, color='Color')) +  geom_text(aes(label=BRAND_MODEL), vjust=1, hjust = 1, size = 5, angle=0) + geom_point(data=centers, aes(x=rate611,y=mean.qoe, color='Center'), size=52, alpha=.3, show_guide=FALSE) + scale_size_area(max_size = 50) + scale_x_continuous(name= "Rate of 611 calls per device type", limits=c(0,2)) + scale_y_continuous(name= "Average Quality of Experience per device type") + theme(legend.position = "none")+ theme(panel.background = element_rect(colour = "pink")) + theme(legend.position = 'none') + guides(fill = guide_legend(keywidth = 9, keyheight = 3))
print(p + ggtitle('Verizon Wireless Device Hotspot identification for 8/1/15') + theme(plot.title = element_text(size=20, face='bold')))



# Boxplots for QoE
boxplot(results5ak$conf ~ results5ak$CALLED_611)
boxplot(results5ak$CALLED_611)

p <- ggplot(device_qoe, aes(y=conf, x=CALLED_611, fill=CALLED_611)) + geom_boxplot() + facet_wrap(~ BRAND_MODEL)
print(p)