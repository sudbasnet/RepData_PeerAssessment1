# Reproducible Research: Peer Assessment 1
---
title: "Reproducible Research: Peer Assessment 1"
author: "Sudeep Basnet" 
output:
html_document:
keep_md: true
---
 
## Loading required library

```r
library(lattice)
```

## Data should be downloaded and the working directry should be set to this location.

## Loading and preprocessing the data
####1. Load the data (i.e. read.csv())
####2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
originalactivity<-read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
activity<-originalactivity[!is.na(originalactivity$steps),]
```
## What is mean total number of steps taken per day?
```
For this part of the assignment, you can ignore the missing values in the dataset.
```
####1. Calculate the total number of steps taken per day

```r
stepstaken<- setNames(aggregate(activity$steps, by = list(activity$date), FUN=sum),c("date","numberofsteps"))
```

####2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(stepstaken$numberofsteps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
####3. Calculate and report the mean and median of the total number of steps taken per day

```r
meansteps <- mean(stepstaken$numberofsteps)
mediansteps <- median(stepstaken$numberofsteps)
meansteps
```

```
## [1] 10766.19
```

```r
mediansteps
```

```
## [1] 10765
```
## What is the average daily activity pattern?

####1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgsteps <- aggregate(x= list(averagesteps = activity$steps), by =list(interval = activity$interval), FUN = mean)
xyplot(avgsteps$averagesteps ~ avgsteps$interval, type = "l", xlab ="5-minute interval", ylab = "the average number of steps taken", main = "Average daily activity pattern")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

####2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgsteps[which.max(avgsteps$averagesteps),"interval"]
```

```
## [1] 835
```
## Imputing missing values
```
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. 
```

####1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
table(is.na(originalactivity$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```
* TRUE means NA

####2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
fulldata <- function(originalactivity){
        for (i in 1:nrow(originalactivity)){
                if(is.na(originalactivity[i,1])){
                        originalactivity[i,"steps"] <- avgsteps[avgsteps$interval == originalactivity[i,"interval"],"averagesteps"] 
                }
        }
        return(originalactivity)
}
```

####3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
filledactivity<- fulldata(originalactivity)
```

####4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
filledstepstaken<- setNames(aggregate(filledactivity$steps, by = list(filledactivity$date), FUN=sum),c("date","numberofsteps"))

histogram(filledstepstaken$numberofsteps, xlab = "Total Steps", ylab = "Frequency", main ="steps taken each day", breaks =  20)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

```r
filledmean <- mean(filledstepstaken$numberofsteps, na.rm = TRUE)
filledmedian <- median(filledstepstaken$numberofsteps, na.rm = TRUE)
```

Median has changed.

## Are there differences in activity patterns between weekdays and weekends?
```
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
```

####1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
daytype <- function(inputday){
        day <- weekdays(as.Date(inputday))
        if(day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){
                return("weekday")
        } else {
                return("weekend")
        } 
}


zzz_data <- filledactivity
zzz_data$typeofday <- sapply(zzz_data$date, FUN = daytype)
```

####2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
simulatedata <- aggregate(steps ~ interval + typeofday, data=zzz_data, mean)

with(simulatedata, xyplot(steps ~ interval | typeofday, type="l", xlab = "Interval", ylab = "Steps", layout = c(1, 2)))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
