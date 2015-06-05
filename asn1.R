library(plyr)
library(dplyr)
library(ggplot2)
d1 <- read.csv("activity.csv")
d1$date <- as.Date(d1$date,"%Y-%m-%d")
#Q1
d3 <- group_by(d1, date)
d5 <- summarize(d3, sum(steps, na.rm=TRUE), mean(steps, na.rm=TRUE))
names(d5) <- c("Date", "Sum", "Mean")
hist(d5$Sum, xlab="", main="")
title(main="Total number of Steps", xlab="Number of Steps")
print("Mean steps per day")
mean(d5$Sum, na.rm=TRUE)
print ("Median steps per day")
median(d5$Sum, na.rm=TRUE)

#Q2
d2 <- d1
d2$interval <- as.POSIXct(sprintf("%04d",d2$interval), format="%H%M")

d4 <- group_by(d2,interval)
d6 <- summarize(d4,sum(steps, na.rm=TRUE),mean(steps, na.rm=TRUE))
names(d6) <- c("Interval", "Sum", "Mean")
plot(d6$Interval, d6$Mean, type="l", main="",xlab="",ylab="Mean Steps", xaxt="n")
title(main="Mean number of steps across hours of a day", xlab="Time interval (hrs)")
axis.POSIXct(side=1, at=window(d6$Interval,deltat=12), format="%H")
print("Interval with maximum steps is ")
max_interval <- d6[d6$Sum==max(d6$Sum),]$Interval

#Q3.1
print("Number of rows with missing values")
sum(is.na(d1$steps))

#Q3.2
#find the index of missing values
missing_values <- which(is.na(d1$steps))
#Missing values in steps filled with Mean steps value of the corresponding
#time interval, averaged over all days.
fill_d1 <- d1
fill_d1$interval <- as.POSIXct(sprintf("%04d",fill_d1$interval), format="%H%M")

for (i in missing_values) {fill_d1$steps[i] <- d6[d6$Interval==fill_d1$interval[i],]$Mean }

#Q3.3
d7 <- group_by(fill_d1, date)
d8 <- summarize(d7,sum(steps),mean(steps))
names(d8) <- c("Date", "Sum", "Mean")
hist(d8$Sum, xlab="", main="")
title(main="Total number of Steps", xlab="Number of Steps")

print("Mean steps per day")
mean(d8$Sum)
print ("Median steps per day")
median(d8$Sum)

#Mean remains the same but the median has slightly changed.

#Q4
mut_d1 <- mutate(fill_d1,day="weekday", value=weekdays(date))
mut_d1[mut_d1$value == "Saturday",]$day <- "weekend"
mut_d1[mut_d1$value == "Sunday",]$day <- "weekend"
d9 <- group_by(mut_d1,day,interval, add=TRUE)
d10 <- summarize(d9,sum(steps),mean(steps))
names(d10) <- c("day","interval","Sum","Mean")

g <- ggplot(d10, aes(x=interval,y=Mean, col=day)) + geom_line(size=1.5) + facet_wrap(~day, ncol=1)
g + scale_x_continuous(breaks = seq(0,2400,100) , labels=c(0:24))