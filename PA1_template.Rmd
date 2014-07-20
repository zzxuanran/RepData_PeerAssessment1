# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
```{r}
setwd("/Users/Rong/Documents/data")
getwd()
data_raw=read.csv("./activity.csv", na.strings="?",colClasses=c("numeric","character","numeric"))
data_raw$date <- strptime(data_raw$date, "%Y-%m-%d")
data<-data_raw[!is.na(data_raw$steps),]
head(data_raw)
```

## What is mean total number of steps taken per day?

```{r}
library(plyr)
daily_step<-ddply(data,.(date), summarise, steps=sum(steps))
hist(daily_step$steps,main="Histogram of daily steps", xlab="the total number of steps taken each day")

# the mean and median total number of steps taken per day
mean(daily_step$steps)
median(daily_step$steps)
```


## What is the average daily activity pattern?

```{r}
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

daily_pattern <- ddply(data, .(interval), summarise, avgsteps = mean(steps))
plot(daily_pattern$interval, daily_pattern$avgsteps, type="l", xlab="5-min interval", ylab="Average steps", main="Average Daily Pattern")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

daily_pattern[daily_pattern$avgsteps==max(daily_pattern$avgsteps),]


```

## Imputing missing values
```{r}
# Number of missing values
sum(is.na(data_raw$steps))

# Fill the NA's with avgstep
mergedata<- arrange(join(data,daily_pattern),interval)

# the new dataset
mergedata$steps[is.na(mergedata$steps)] <- mergedata$avesteps[is.na(mergedata$steps)]
# plot the histogram
new_daily_steps <- ddply(mergedata, .(date), summarise, steps = sum(steps))
hist(new_daily_steps$steps, main = "Number of Steps", xlab = "steps taken each day",  )

# mean and median total number of steps taken per day don't change
# significantly
mean(new_daily_steps$steps)
median(new_daily_steps$steps)

daily_steps_1 <- sum(data$steps)
daily_steps_2 <- sum(mergedata$steps)
diff <- daily_steps_2 - daily_steps_1[]

#Mean values didn't change as imputation used the average on 5-mi interval

```



## Are there differences in activity patterns between weekdays and weekends?

```{r}
library(lattice)
weekdays <- weekdays(as.Date(mergedata$date))
data_weekdays <- transform(mergedata, day = weekdays)
data_weekdays$wk <- ifelse(data_weekdays$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
average_week <- ddply(data_weekdays, .(interval, wk), summarise, steps = mean(steps))

xyplot(steps ~ interval | wk, data = average_week, layout = c(1, 2), type = "l")
```