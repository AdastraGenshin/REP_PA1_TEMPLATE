---
output:
  html_document:
    keep_md: yes
---
###Peer Assessment 1

##Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis


```r
#1
setwd("~/Git/R/reproducible_research/Assesment/a1/")
data_a1 <- read.csv("activity.csv")
str(data_a1[,"date"])
```

```
##  Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
```
##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


Calculate and report the mean and median of the total number of steps taken per day

```r
#2.1
steps_per_day_total <- tapply(data_a1$steps,data_a1$date,sum,na.rm=TRUE)
steps_per_day_total
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

```r
#2.2
hist(steps_per_day_total)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
#2.3
median_a1 <- median(steps_per_day_total)
median_a1
```

```
## [1] 10395
```

```r
mean_a1 <- mean(steps_per_day_total)
mean_a1
```

```
## [1] 9354.23
```
##What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
#3.1
steps_of_interval_avaraged_across_all_days<- tapply(data_a1$steps,data_a1$interval,sum,na.rm=TRUE)/61
```

```r
plot(steps_of_interval_avaraged_across_all_days,type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
#3.2
interval_max <- names(steps_of_interval_avaraged_across_all_days[steps_of_interval_avaraged_across_all_days==max(steps_of_interval_avaraged_across_all_days)])
```
Answer: the 5-minute interval contains the maximum number of steps is 835.

##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
#4.1
miss_num <- sum(is.na(data_a1$steps))
```
Answer: the total number of missing values in the dataset is 2304.

```r
#4.2 4.3
#imputing missing values with the mean for that 5-minute interval
for(i in seq_len(nrow(data_a1))) {
  if(is.na(data_a1$steps[i])){
    temp <- floor(data_a1$interval[i]/100)*12+(data_a1$interval[i]%%100)/5+1
    data_a1$steps[i]<- steps_of_interval_avaraged_across_all_days[temp]
  }
}
#4.4
steps_per_day_total <- tapply(data_a1$steps,data_a1$date,sum,na.rm=TRUE)
```

```r
hist(steps_per_day_total)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
median_a1 <- median(steps_per_day_total)
median_a1
```

```
## [1] 10395
```

```r
mean_a1 <- mean(steps_per_day_total)
mean_a1
```

```
## [1] 10581.01
```
Answer: these values differ from the estimates from the first part of the assignment. the impact of imputing missing data on the estimates of the total daily number of steps is improving the frequency of those intervals with more missing values.


##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels 鈥<93> 鈥渨eekday鈥<9d> and 鈥渨eekend鈥<9d> indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
#5.1
pattern <- seq_len(nrow(data_a1))
NANANA
```

```
## [1] NA
```

```r
NANANA
```

```
## [1] NA
```

```r
pattern <- as.factor(pattern)
str(pattern)
```

```
##  Factor w/ 17568 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
```

```r
data_a1 <- cbind(data_a1,pattern)
d_d<- sum(data_a1$pattern=="weekday")/288 #count days of weekdays
d_e<- sum(data_a1$pattern=="weekend")/288 #count days of weekends
```

```r
#5.2
steps_of_interval_avaraged_across_all_days_weekday<- tapply(data_a1$steps[data_a1$pattern=="weekday"],data_a1$interval[data_a1$pattern=="weekday"],sum,na.rm=TRUE)/d_d
```

```r
plot(steps_of_interval_avaraged_across_all_days_weekday,type="l",ylab="weekday")
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Error in plot.window(...): 'xlim'值不能是无限的
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

```r
steps_of_interval_avaraged_across_all_days_weekend<- tapply(data_a1$steps[data_a1$pattern=="weekend"],data_a1$interval[data_a1$pattern=="weekend"],sum,na.rm=TRUE)/d_e
```

```r
plot(steps_of_interval_avaraged_across_all_days_weekend,type="l",ylab="weekend")
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Error in plot.window(...): 'xlim'值不能是无限的
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 
