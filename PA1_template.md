w1\_assignment
================
Owen
7 July 2019

Reproducible Research - Week 2 - Course Project 1
=================================================

Introduction
------------

This document contains all the code and graphs for the assignments.

### Loading and preprocessing the data

#### 1. Load the data (i.e. read.csv())

#### 2. Process/transform the data (if necessary) into a format suitable for your analysis

``` r
df <- read.csv(file = paste0(getwd(),'/repdata_data_activity/activity.csv'))
head(df)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

### What is mean total number of steps taken per day?

#### 1. Calculate the total number of steps taken per day

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
df_1 <-
        df %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(total_steps = sum(steps))
df_1 <- as.data.frame(df_1)
head(df_1)
```

    ##         date total_steps
    ## 1 2012-10-02         126
    ## 2 2012-10-03       11352
    ## 3 2012-10-04       12116
    ## 4 2012-10-05       13294
    ## 5 2012-10-06       15420
    ## 6 2012-10-07       11015

#### 2. Make a histogram of the total number of steps taken each day

``` r
hist(df_1$total_steps)
```

![](PA1_template_files/figure-markdown_github/histogram-1.png)

#### 3. Calculate and report the mean and median of the total number of steps taken per day

``` r
mean(df_1$total_steps)
```

    ## [1] 10766.19

``` r
median(df_1$total_steps)
```

    ## [1] 10765

### What is the average daily activity pattern?

#### 1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
library(dplyr)
df_2 <-
        df %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarise(avg_steps = mean(steps))
df_2 <- as.data.frame(df_2)
plot(df_2$interval,df_2$avg_steps,type = 'l')
```

![](PA1_template_files/figure-markdown_github/time%20series-1.png)

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
df_2[which.max(df_2$avg_steps),]
```

    ##     interval avg_steps
    ## 104      835  206.1698

### Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
sum(is.na(df))
```

    ## [1] 2304

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy: filling in the missing value with the mean of that 5-minute interval

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
df_fill <-
        df %>%
        group_by(interval) %>%
        mutate(mean_interval = mean(steps, na.rm = TRUE),
               steps = ifelse(is.na(steps),mean_interval,steps)) %>%
        select(-mean_interval)
```

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
df_fill_agg <-
        df_fill %>%
        group_by(date) %>%
        summarise(total_steps = sum(steps))


hist(df_fill_agg$total_steps)
```

![](PA1_template_files/figure-markdown_github/histogram%20after%20fill-1.png)

``` r
mean(df_fill_agg$total_steps)
```

    ## [1] 10766.19

``` r
median(df_fill_agg$total_steps)
```

    ## [1] 10766.19

By comparing between those 2 histograms, we can clearly see that there were 25 days falling into the 10,000 ~ 15,000 but now there are 35 days.The mean is the same as the first part but median is different, the median was lower than mean but now they are the same, which means that filling in missing value will have an impact on median of total daily number of steps.

### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
df_fill_week <-
        df_fill %>%
        mutate(date = as.Date(date),
               weekday = as.factor(ifelse(weekdays(date) %in% c('Satuarday','Sunday'),'weekend','weekday')))
```

#### 2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
library(dplyr)
library(lattice)

df_fill_week_agg <-
        df_fill_week %>%
        group_by(weekday,interval) %>%
        summarise(avg_steps = mean(steps))

xyplot(data = df_fill_week_agg,avg_steps~interval|weekday, type = 'b')
```

![](PA1_template_files/figure-markdown_github/time%20series%20weekday-1.png)
