---
title: "Reproducible Research: Peer Assessment 1"
author: "Manish Khaware"
date: "February 9, 2020"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

Code for reading in the dataset 

```r
activity <- read.csv("./activity.csv")
```
Process the data

```r
dim(activity)
```

```
## [1] 17568     3
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#total number of missing data
sum(is.na(activity$steps))/dim(activity)[[1]]
```

```
## [1] 0.1311475
```

```r
pairs(activity)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day


```r
stepsPerDay <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
```
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(stepsPerDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day

```r
stepsPerDayMean <- mean(stepsPerDay$steps)
stepsPerDayMean
```

```
## [1] 10766.19
```

```r
stepsPerDayMedian <- median(stepsPerDay$steps)
stepsPerDayMedian
```

```
## [1] 10765
```

# What is the average daily activity pattern?

Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsPerInterval<-aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalWithMaxNoSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNoSteps
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
totalMissingsValues <- sum(is.na(activity$steps))
totalMissingsValues
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
getMeanStepsPerInterval<-function(interval){
    stepsPerInterval[stepsPerInterval$interval==interval,]$steps
} 
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityWithNoNA<-activity
for(i in 1:nrow(activityWithNoNA)){
    if(is.na(activityWithNoNA[i,]$steps)){
        activityWithNoNA[i,]$steps <- getMeanStepsPerInterval(activityWithNoNA[i,]$interval)
    }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityWithNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityWithNoNA$date <- as.Date(strptime(activityWithNoNA$date, format="%Y-%m-%d"))
activityWithNoNA$day <- weekdays(activityWithNoNA$date)
for (i in 1:nrow(activityWithNoNA)) {
    if (activityWithNoNA[i,]$day %in% c("Saturday","Sunday")) {
        activityWithNoNA[i,]$day<-"weekend"
    }
    else{
        activityWithNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityWithNoNA$steps ~ activityWithNoNA$interval + activityWithNoNA$day, activityWithNoNA, mean)
```

Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
