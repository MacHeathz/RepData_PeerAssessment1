# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(readr)
library(lubridate)
activity_monitoring_data <- read_csv('activity.csv', col_types = "ici")
activity_monitoring_data <- transform(activity_monitoring_data, date = ymd(date))
```


## What is the mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

 1. Calculate the total number of steps taken per day.
 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.
 3. Calculate and report the mean and median of the total number of steps taken per day.

### 1 & 2

```r
total.steps <- tapply(activity_monitoring_data$steps, activity_monitoring_data$date, FUN = sum, na.rm = TRUE)
hist(total.steps, main = "Histogram of Total Number of Steps per Day", xlab = "Total Number of Steps per Day")
rug(total.steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean <- as.integer(mean(total.steps, na.rm = TRUE))
median <- as.integer(median(total.steps, na.rm = TRUE))
```

### 3
The mean number of steps per day is 9354.

The median number of steps per day is 10395.

## What is the average daily activity pattern?
 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

### 1

```r
Intervals <- unique(activity_monitoring_data$interval)
Activity <- tapply(activity_monitoring_data$steps, activity_monitoring_data$interval, FUN = mean, na.rm = TRUE)
interval_activity <- data.frame(Interval = Intervals, Activity = Activity)
plot(interval_activity, type = "l", main = "Mean Activity per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
interval_activity <- interval_activity[order(-Activity),]
max <- interval_activity[1,]$Interval
```
### 2
The maximum number of steps on average, across all days, is in the interval: 835.

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

### 1

```r
NA_rows <- sum(is.na(activity_monitoring_data))
```

### 2
The dataset has 2304 rows containing missing values.

Let's look at the dataset where the NA's are:

```r
summary(activity_monitoring_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```
So we see that the missing values are only in the steps column. Let's use the `impute.knn` function from the [biocLite](https://www.bioconductor.org/packages/release/bioc/html/impute.html) package. We essentially do this by looking at similar values (k-nearest neighbors), considering the 5-minute intervals.

### 3

```r
library(impute)
impute_matrix <- as.matrix(data.frame(activity_monitoring_data$steps, activity_monitoring_data$interval))
imputed_result <- impute.knn(impute_matrix)
```

```
## Cluster size 17568 broken into 8784 8784 
## Cluster size 8784 broken into 4445 4339 
## Cluster size 4445 broken into 2250 2195 
## Cluster size 2250 broken into 1111 1139 
## Done cluster 1111 
## Done cluster 1139 
## Done cluster 2250 
## Cluster size 2195 broken into 862 1333 
## Done cluster 862 
## Done cluster 1333 
## Done cluster 2195 
## Done cluster 4445 
## Cluster size 4339 broken into 2195 2144 
## Cluster size 2195 broken into 980 1215 
## Done cluster 980 
## Done cluster 1215 
## Done cluster 2195 
## Cluster size 2144 broken into 2044 100 
## Cluster size 2044 broken into 1174 870 
## Done cluster 1174 
## Done cluster 870 
## Done cluster 2044 
## Done cluster 100 
## Done cluster 2144 
## Done cluster 4339 
## Done cluster 8784 
## Cluster size 8784 broken into 4401 4383 
## Cluster size 4401 broken into 2275 2126 
## Cluster size 2275 broken into 2007 268 
## Cluster size 2007 broken into 845 1162 
## Done cluster 845 
## Done cluster 1162 
## Done cluster 2007 
## Done cluster 268 
## Done cluster 2275 
## Cluster size 2126 broken into 928 1198 
## Done cluster 928 
## Done cluster 1198 
## Done cluster 2126 
## Done cluster 4401 
## Cluster size 4383 broken into 2187 2196 
## Cluster size 2187 broken into 976 1211 
## Done cluster 976 
## Done cluster 1211 
## Done cluster 2187 
## Cluster size 2196 broken into 1220 976 
## Done cluster 1220 
## Done cluster 976 
## Done cluster 2196 
## Done cluster 4383 
## Done cluster 8784
```

```r
imp_activity_monitoring_data <- data.frame(steps = imputed_result$data[,1],
                                           date = activity_monitoring_data$date,
                                           interval = activity_monitoring_data$interval)
summary(imp_activity_monitoring_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 35.18   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 18.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

### 4

```r
total.steps <- tapply(imp_activity_monitoring_data$steps, imp_activity_monitoring_data$date, FUN = sum)
hist(total.steps, main = "Histogram of Total Number of Steps per Day (imputed)", xlab = "Total Number of Steps per Day")
rug(total.steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
mean <- as.integer(mean(total.steps))
median <- as.integer(median(total.steps))
```

The mean number of steps per day is 10131.

The median number of steps per day is 10395.

Comparing these results to the mean and median from the first question, we find that the median is the same but the mean has changed and is closer to the median now. In the histogram too, we see that the distribution is better.

## Are there differences in activity patterns between weekdays and weekends?
For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

 1. Create a new factor variable in the dataset with two levels  weekday and weekend indicating whether a given date is a weekday or weekend day.
 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

### 1

```r
imp_activity_monitoring_data[,"day.type"] <-
        sapply(imp_activity_monitoring_data$date, FUN = function(d){
                ifelse(wday(d) == 1 | wday(d) == 7, "weekend", "weekday")
        })
```

### 2

```r
Intervals <- unique(imp_activity_monitoring_data$interval)

weekends <- subset(imp_activity_monitoring_data, day.type == "weekend")
weekend_activity <- tapply(weekends$steps, weekends$interval, FUN = mean)
Weekends <- data.frame(Interval = Intervals, Activity = weekend_activity)

weekdays <- subset(imp_activity_monitoring_data, day.type == "weekday")
weekday_activity <- tapply(weekdays$steps, weekdays$interval, FUN = mean)
Weekdays <- data.frame(Interval = Intervals, Activity = weekday_activity)

par(mfrow=c(2,1))
plot(Weekdays, type = "l", ylab = "Number of Steps", main = "weekday")
plot(Weekends, type = "l", ylab = "Number of Steps", main = "weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 
