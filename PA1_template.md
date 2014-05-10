# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
To load the data, we first  unzip he activity.zip file containing the dataset. 
That can be done directly from R. There are packages that let you directly
load data from a compressed file. We are not using them here. 


```r
unzip("activity.zip")
```


If that worked, we should now have the actitivy.csv file in our working 
directory. We now read the data in. We will convert all dates factors to 
the Date class. It will allow us to use date related operation if necessary. 


```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```


Let's see what our data looks like (first 6 samples)


```r
head(data)
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


## What is mean total number of steps taken per day?
Let's explore the mean and median for the daily number of steps


```r
steps.per.day <- aggregate(steps ~ date, data, sum)
mean(steps.per.day$steps)
```

```
## [1] 10766
```

```r
median(steps.per.day$steps)
```

```
## [1] 10765
```


Let's look at the big picture with a histogram.   

```r
hist(steps.per.day$steps, main = "Histogram of number of steps per day", xlab = "Steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 



## What is the average daily activity pattern?


```r
mean.steps.per.interval <- aggregate(steps ~ interval, data, mean)
plot(mean.steps.per.interval, type = "l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


What it the interval with most steps in average, indicated by that high peak in 
the plot?


```r
mean.steps.per.interval[order(-mean.steps.per.interval$steps), ][1, "interval"]
```

```
## [1] 835
```


## Imputing missing values
To obtain the number of missing values, we count how many NAs we have in the 
steps column.


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```



```r
NAs <- mean.steps.per.interval[order(-mean.steps.per.interval$steps), ][1, "interval"]
NAs_proportion <- NAs/nrow(data)
```


We've got 835 missing "steps" values out of the 17568 samples taken.
That means we almost have  5% of missing data. 

Let's fill all missing values with the interval mean (using all other data we 
have for each interval).


```r
data.complete <- data
for (i in 1:nrow(mean.steps.per.interval)) {
    interval = mean.steps.per.interval[i, "interval"]
    i_mean = mean.steps.per.interval[i, "steps"]
    data.complete[(data.complete$interval == interval) & (is.na(data.complete$steps)), 
        "steps"] <- i_mean
}
sum(is.na(data.complete$steps))
```

```
## [1] 0
```


Let' see if that changes our centrality measures.


```r
steps.per.day.completed <- aggregate(steps ~ date, data.complete, sum)
mean(steps.per.day.completed$steps)
```

```
## [1] 10766
```

```r
median(steps.per.day.completed$steps)
```

```
## [1] 10766
```


Let's look at the big picture with a histogram.   

```r
par(mfrow = c(1, 2))
hist(steps.per.day$steps, main = "Steps per day (with NAs)", xlab = "Steps per day")
hist(steps.per.day.completed$steps, main = "Steps per day (no NAs)", xlab = "Steps per day")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


That small amount of missing data **did not** have a big impact on centrallity 
measures.

## Are there differences in activity patterns between weekdays and weekends?
Let's indicate, in a new column of our dataframe, wether the sample occured on 
a weekday or a weekend. 


```r
weekends <- which(weekdays(data.complete$date) %in% c("Saturday", "Sunday"))
data.complete$dayofweek <- "Weekday"
data.complete[weekends, "dayofweek"] <- "Weekend"
data.complete$dayofweek <- as.factor(data.complete$dayofweek)
summary(data.complete$dayofweek)
```

```
## Weekday Weekend 
##   12960    4608
```


Now, let's compare daily activity for weekdays and weekends


```r

par(mfrow = c(2, 1))
weekend.activity <- aggregate(steps ~ interval, subset(data.complete, data.complete$dayofweek == 
    "Weekend"), mean)
weekday.activity <- aggregate(steps ~ interval, subset(data.complete, data.complete$dayofweek == 
    "Weekday"), mean)

plot(weekend.activity, type = "l", main = "Weekend")
plot(weekday.activity, type = "l", main = "Weekday")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


