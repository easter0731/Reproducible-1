## The purpose of this study is to
## To address this problem we attempt to 


## Fist, we download our dataset from course web site, which sonsists of months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day
## 
## "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
## And then loaded it into R using R script as shown below.
library(ggplot2)
library("plyr")

activity <- read.csv("activity.csv")

## total number of steps taken per day
steps_activity <- activity[!is.na(activity$steps),]

## calculate the total steps by grouping according to date
dailysteps <- ddply(steps_activity, .(date), summarise, total_steps=sum(steps))
## plot
g <- ggplot(data=dailysteps, aes(total_steps))
g + geom_histogram (fill="steelblue", color="blue")

summary <- summary(dailysteps$total_steps)


## What is the average daily activity pattern?
meaninterval <- ddply (steps_activity, .(interval), summarise, meansteps=mean(steps))
max <-meaninterval$meansteps==max(meaninterval$meansteps)
with(meaninterval, plot(x=interval, y=meansteps, type="l"))
points(x=meaninterval$interval[max], y=meaninterval$meansteps[max], col="Red",pch=5)

meaninterval[max,]

## Imputing missing values
na <- is.na(activity$steps)
sum (na)
## We can fill all of the missing values in the dataset with the mean of the corresponding interval.
steps_activity2 <- activity

for (i in seq_len(nrow(activity))) {
        if(is.na(activity$steps[i])) {
                steps_activity2[i,1]= meaninterval[meaninterval$interval==steps_activity2[i,3],2]
        }
        
}


dailysteps2 <- ddply(steps_activity2, .(date), summarise, total_steps=sum(steps))
summary(dailysteps2$total_steps)


dailysteps$na <- "without NA"
dailysteps2$na <- "with NA"
daily <- rbind(dailysteps,dailysteps2)
daily$na <- as.factor(daily$na)

g <- ggplot(data=daily, aes(total_steps))
g + geom_histogram (aes(fill=na)) + facet_grid(. ~na)

##Are there differences in activity patterns between weekdays and weekends?
Sys.setlocale("LC_TIME", "English")
steps_activity2$day <- weekdays(as.Date(steps_activity2$date))

for (i in seq_len(nrow(steps_activity2))) {
        if (steps_activity2[i,4] %in% "Saturday" | steps_activity2[i,4] %in% "Sunday" )  {
                steps_activity2[i,4] <- "Weekend"
        } else {steps_activity2[i,4] <- "Weekday"
        }
}

steps_activity2$day<-as.factor(steps_activity2$day)
        
meaninterval2 <- ddply (steps_activity2, .(day, interval), summarise, meansteps=mean(steps))        
        
g <- ggplot (meaninterval2, aes(interval, meansteps))
g+geom_line(aes(color=day))