# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(dplyr)
```

```r
activity <- read.csv("activity/activity.csv")
activity$date <- as.POSIXct(activity$date)
```
##Grouping data for later manipulation

```r
act_by_date <- group_by(activity, date)
act_by_interval <- group_by(activity, interval)
```

## Mean total number of steps taken per day = 10766.19
## Median total number of steps taken per day = 10765


```r
total_steps_per_day <- summarize(act_by_date, sum_steps = sum(steps))
hist(total_steps_per_day$sum_steps, xlab = "Steps per Day", main = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
total_mean <- mean(total_steps_per_day$sum_steps, na.rm = TRUE)
total_median <- median(total_steps_per_day$sum_steps, na.rm = TRUE)
```

```r
total_mean
```

```
## [1] 10766.19
```

```r
total_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?
##Peaks in average activity occur from 8:00 to 9:00 A.M., around noon and from 4:00 to 6:00 P.M..
 

```r
mean_per_interval <- summarize(act_by_interval, imean = mean(steps, na.rm = TRUE))
 plot(mean_per_interval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

##The five minute interval with the highest average number of steps is between   8:35 and 8:40 A.M.


```r
top_interval <- filter(mean_per_interval, imean == max(imean))$interval
top_interval
```

```
## [1] 835
```

##The total number of missing values = 2304

```r
total_incomplete <- sum(!complete.cases(activity))
total_incomplete
```

```
## [1] 2304
```
##Missing step values are replaced by the mean of reported step values for the interval with the missing value.

```r
indexes <- sapply(activity$steps, is.na)

for(i in 1:length(indexes)) 
  if(indexes[i]) {
      activity[i,]$steps <- mean_per_interval[which(mean_per_interval$interval 
      == activity[i,]$interval),]$imean}
```

##Updated histogram with missing values imputted

```r
act_by_date <- group_by(activity, date)
total_steps_per_day <- summarize(act_by_date, sum_steps = sum(steps))
hist(total_steps_per_day$sum_steps, xlab = "Steps per Day", main = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

## Are there differences in activity patterns between weekdays and weekends?
## The mean activity on weekends is spaced more evenly throughout the day.

```r
day_type <- factor(c("weekday", "weekend"))
activity <- mutate(activity, day_type = "weekday")
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

for(i in 1:length(activity$date)) 
  {if(weekdays(activity[i,]$date) %in% weekday)
      {activity[i,]$day_type <- "weekday"} 
   else {
      activity[i,]$day_type <- "weekend"}
   }

act_by_int_date <- group_by(activity, interval, day_type)

combined_means <- summarize(act_by_int_date, imean = mean(steps))
```
## Panel plot of activity patterns for weekdays and weekends


```r
library(ggplot2)

wplot <- ggplot(combined_means, aes(interval, imean)) + facet_grid(day_type ~ .) + geom_line() + ylab("Mean Steps") + ggtitle("Mean Steps per Intervals for Weekends and Weekdays")

wplot
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
