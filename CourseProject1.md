---
output: 
  html_document: 
    keep_md: yes
---
Coursera - Reproducible Research - Project 1 
----------------

1. Code for reading in the dataset and processing the data.


```r
    # read in and unzip the data set 
    data_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(data_url,"activity.zip")
    unzip(zipfile="./activity.zip")
    activity <- read.csv("activity.csv")
    activity$date <- as.Date(activity$date,format="%Y-%m-%d") #format the date column
    activity$weekday <- weekdays(activity$date) #add a column for the weekday
    
    #Create several cuts of the raw data to be used later:
    
    #Average number of steps per interval, first removing intervals with NAs
        activity.complete <- activity[complete.cases(activity),]
        interval_steps <- aggregate(activity.complete$steps,by=list(interval=activity.complete$interval),FUN=mean)
    
    #Average number of steps per weekday interval
        activityWD <- activity.complete[!(activity.complete$weekday %in% c("Saturday","Sunday")),]
        intervalWD <- aggregate(activityWD$steps,by=list(interval=activityWD$interval),FUN=mean)
        
        activityWE <- activity.complete[activity.complete$weekday %in% c("Saturday","Sunday"),]
        intervalWE <- aggregate(activityWE$steps,by=list(interval=activityWE$interval),FUN=mean)
    
    #Number of steps per day, based on raw data with NAs not removed
        daily_steps <- aggregate(activity$steps,by=list(Date = activity$date),FUN = sum)
        daily_steps$Date <- as.Date(daily_steps$Date,format="%Y-%m-%d")
```
2. Histogram of the amount of steps each day.


```r
    hist(daily_steps$x,main="Daily number of Steps",xlab="Steps",col="green",breaks=length(daily_steps$Date))
```

![](CourseProject1_files/figure-html/hist-1.png)<!-- -->

3. Mean and median number of steps taken each day:

mean:

```r
   mean(daily_steps$x,na.rm=TRUE)
```

```
## [1] 10766.19
```
median:

```r
    median(daily_steps$x,na.rm=TRUE)
```

```
## [1] 10765
```

4. Time series plot of the average number of steps taken

Note: The question was somewhat vague.  I took this to mean the average number of steps taken per interval.


```r
    with(interval_steps,plot(interval,x,type="l",main="Average number of steps per interval",xlab="Interval",ylab="steps"))
```

![](CourseProject1_files/figure-html/time_series-1.png)<!-- -->

5. The 5-minute inteval that, on average, contains the maximum number of steps:

```r
    max <- max(interval_steps$x)
    max_interval <- interval_steps[interval_steps$x == max,]
    max_interval
```

```
##     interval        x
## 104      835 206.1698
```

6. Code to describe and show a strategy for imputing missing data

    Loop through original data set replacing NAs with the mean for that interval
    

```r
        activity.impute <- activity  ## First make a copy of the original activity dataset

        for(i in seq_along(activity$steps)){  ##loop through each interval observation
            
            if (is.na(activity[i,"steps"])){  ## if it's NA, replace it with the mean steps for that interval
                
                interval <- activity[i,"interval"]
                activity.impute[i,"steps"] <- interval_steps[interval_steps$interval == interval,"x"]
            }
        }
```


7. Histogram of the total number of steps taken each day after missing values are imputes

```r
        daily_steps.impute <- aggregate(activity.impute$steps,by=list(Date=activity.impute$date),FUN=sum)
        hist(daily_steps.impute$x,main="Daily number of steps (imputed)",xlab="steps",col="blue",breaks=length(daily_steps.impute$Date))
```

![](CourseProject1_files/figure-html/hist2-1.png)<!-- -->

Mean of the new data set with imputes values:

```r
        mean(daily_steps.impute$x)
```

```
## [1] 10766.19
```
Median of the new data set with imputed values:

```r
        median(daily_steps.impute$x)
```

```
## [1] 10766.19
```

EXTRA: Panel plot comapring the histogram with raw data vs the one with imputed data. 


```r
    par(mfrow=c(1,2))
    nbreaks <- length(daily_steps$Date)
    nbreaks <- 20

    hist(daily_steps$x,main="Daily steps (raw data)",xlab="Steps",col="green",breaks=nbreaks,ylim=c(0,12),xlim=c(0,25000))
    hist(daily_steps.impute$x,main="Daily steps (values imputed)",xlab="steps",col="blue",breaks=nbreaks,ylim=c(0,12),xlim=c(0,25000))
```

![](CourseProject1_files/figure-html/panels-1.png)<!-- -->

8. Panel plot comparing the number of steps taken per 5-minute interval across weekdays and weekends


```r
    par(mfrow=c(1,2))
    ymax <- max(max(intervalWD$steps),max(intervalWE$steps))
```

```
## Warning in max(intervalWD$steps): no non-missing arguments to max;
## returning -Inf
```

```
## Warning in max(intervalWE$steps): no non-missing arguments to max;
## returning -Inf
```

```r
    with(intervalWD,plot(interval,x,type="l",main="Avg steps/interval - Weekdays",xlab="Interval",ylab="Steps",ylim=c(0,250)))
    with(intervalWE,plot(interval,x,type="l",main="Avg steps/interval - Weekends",xlab="Interval",ylab="Steps",ylim=c(0,250)))
```

![](CourseProject1_files/figure-html/weekdays-1.png)<!-- -->








