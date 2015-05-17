---
title: "Activity Monitoring Data"
author: "Peter Blane"
date: "May 16, 2015"
output: html_document
---

This R Markdown document is meant to follow the original outline structure of the project assignment description.  The below will include the sections and questions in the same order they were presented by the instructor.  The difference is that I will add my solution(s) beneath each section/question to depict my results.

---

## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
setwd("~/Documents/Coursera Data Science/Course 5")
d <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
dna <- na.omit(d)
steps <- aggregate(steps ~ date, data = dna, sum)
hist(steps$steps, main = "Total Steps per Day", xlab = "Steps", col = "blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(steps$steps)
```

```
## [1] 10766.19
```

```r
median(steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg <- tapply(dna$steps, dna$interval, mean)
plot(row.names(avg), avg, type = "l", main = "Average Number of Steps Taken",xlab = "5 Minute Interval", 
     ylab = "Average Across All Days", col = "blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
MaxSteps <- which.max(avg)
names(MaxSteps)
```

```
## [1] "835"
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totalna <- sum(is.na(d))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
NAfix <- which(is.na(d$steps))
newStep <- rep(mean(d$steps, na.rm = TRUE), times = length(NAfix))
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
d[NAfix, "steps"] <- newStep
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_steps <- aggregate(steps ~ date, data = d, sum)

mean(total_steps$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps$steps)
```

```
## [1] 10766.19
```


```r
hist(total_steps$steps, 
     breaks=seq(from=0, to=25000, by=2500),
     col="green", 
     xlab="Total Number of Steps", 
     ylim=c(0, 30), 
     main="Total Number of Steps Taken per day (NA's replaced by mean value)")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
d$date <- as.POSIXct(d$date, format="%Y-%m-%d")
d_week <- data.frame(date = d$date, 
                      weekday = tolower(weekdays(d$date)), 
                      steps = d$steps, 
                      interval = d$interval)
d_week <- cbind(d_week, 
                 day_type=ifelse(d_week$weekday == "saturday" | 
                                d_week$weekday == "sunday", "weekend", "weekday"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
avg_steps <- aggregate(d_week$steps, by=list(d_week$day_type, d_week$weekday, d_week$interval), mean)
names(avg_steps) <- c("Day.Type", "Day.of.Week", "Interval", "Mean")
library(lattice)
xyplot(Mean ~ Interval | Day.Type, avg_steps, 
        type = "l",  
        xlab="Interval", 
        ylab="Number of steps", 
        layout=c(1,2))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
