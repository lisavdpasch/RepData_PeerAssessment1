---
title: 'Reproducible Research | Peer-graded Assignment: Course Project 1'
author: "Lisa van der Pasch"
output: 
  html_document: 
    keep_md: yes
---

##Loading and preprocessing the data
Show any code that is needed to

###1. Load the data (i.e. read.csv())


```r
setwd("/Volumes/Naamloos/Users/lisavanderpasch/Downloads")
activity <- read.csv('activity.csv')
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
```

###2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

###1. Make a histogram of the total number of steps taken each day


```r
activityDay <- group_by(activity,date)
stepsDay <- summarise(activityDay, totalSteps=sum(steps))
hist(stepsDay$totalSteps, main='Total number of steps per day', xlab='Number of steps', breaks=30)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

###2. Calculate and report the mean and median total number of steps taken per day


```r
meanSteps <- as.integer(mean(stepsDay$totalSteps, na.rm=TRUE),digits=0)
medianSteps <- as.integer(median(stepsDay$totalSteps, na.rm=TRUE))
```

* Mean total number of steps taken per day: 10766
* Median total number of steps taken per day: 10765

##What is the average daily activity pattern?
###1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
timeSeries <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

plot(row.names(timeSeries), timeSeries, type = "l", xlab = "5-minute interval", 
    ylab = "Average number of steps taken", main = "Average number of steps taken, averaged across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxInterval <- names(which.max(timeSeries))
```

* Maximum number of steps: 835

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totalNA <- sum(is.na(activity))
```

* Total number of missing values: 2304

###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
mean(activity$steps,na.rm=TRUE)
```

```
## [1] 37.3826
```

###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity2 <- activity
activity2$steps[is.na(activity2$steps)] <- mean(activity2$steps, na.rm = T)
```

###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activityDay2 <- group_by(activity2,date)
stepsDay2 <- summarise(activityDay2, totalSteps2=sum(steps))
hist(stepsDay2$totalSteps2, main='Total number of steps per day', xlab='Number of steps', breaks=30)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
meanSteps2 <- as.integer(mean(stepsDay2$totalSteps2, na.rm=TRUE),digits=0)
medianSteps2 <- as.integer(median(stepsDay2$totalSteps2, na.rm=TRUE))
```

* Mean total number of steps taken per day: 10766
* Median total number of steps taken per day: 10766

There are no big differences

##Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

###1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity2$weekdays <- weekdays(activity2$date)

activity2$week[(activity2$weekdays == "Saturday" | activity2$weekdays == "Sunday")] <- "weekend"
activity2$week[!(activity2$week == "weekend")] <- "weekday"
```

###2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data.


```r
week <- ddply(activity2, c("interval", "week"), function(x) apply(x[1], 2, mean))

xyplot(steps ~ interval | week, data = week, type = "l", xlab = "5-minute interval", 
    ylab = "Average number of steps taken", layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
