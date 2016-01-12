# Reproducible Research: Peer Assessment 1

```r
library("ggplot2")
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

## Loading and preprocessing the data

Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())

```r
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day

```r
steps_by_date <- aggregate(steps ~ date, data, sum)
```
2.Make a histogram of the total number of steps taken each day

```r
qplot(steps_by_date$steps, xlab="Day", ylab="Total steps per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\
3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_by_date$steps)
```

```
## [1] 10766.19
```

```r
median(steps_by_date$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1.Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
time_series <- aggregate(steps ~ interval, data, mean, na.rm= TRUE)
ggplot(time_series, aes(x=interval, y=steps)) + geom_line() + labs(x="Interval" ,y="Mean of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)\
2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
time_series[which.max(time_series$steps),]$steps
```

```
## [1] 206.1698
```

## Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the
mean/median for that day, or the mean for that 5-minute interval, etc.

```r
data_new <- data

impute <- function(steps, interval){
    if (is.na(steps)){
        return(time_series[time_series$interval == interval, "steps"])
    }else{
        return(steps)
    }
}
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_new$steps <- mapply(impute, data_new$steps, data_new$interval)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_by_date_new <- aggregate(steps ~ date, data_new, sum)
qplot(steps_by_date$steps, xlab="Day", ylab="Total steps per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)\

```r
mean(steps_by_date_new$steps)
```

```
## [1] 10766.19
```

```r
median(steps_by_date_new$steps)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data_new$day <- factor((weekdays(as.Date(data_new$date)) %in% c("Saturday", "Sunday")), levels=c(TRUE, FALSE), labels=c('Weekend', 'Weekday'))
```

2.Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
time_series_new <- aggregate(steps ~ interval + day, data=data_new, mean)
ggplot(time_series_new, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(day ~ .) +
    xlab("5 min Interval") + 
    ylab("mean of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)\
