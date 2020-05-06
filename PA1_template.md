Introduction
------------

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
<a href="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" class="uri">https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip</a>

The variables included in this dataset are:

-   **steps** : Number of steps taking in a 5-minute interval (missing
    values are coded as NA)
-   **date** : The date on which the measurement was taken in YYYY-MM-DD
    format
-   **interval**: Identifier for the 5-minute interval in which
    measurement was taken

``` r
#Loading the data and libraries
library(ggplot2)
library(lattice)

Activity <- read.csv("activity.csv")
head(Activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

Now we shall proceed to answer all of the questions from the assignment.
\#\#\# What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day.

``` r
sumTable <- rowsum(Activity$steps, group = Activity$date, na.rm =TRUE )
df <- data.frame(cbind(row.names(sumTable), sumTable))
names(df) <- c("date", "steps") 
df$steps <- as.numeric(levels(df$steps))[df$steps]
head(df)
```

    ##                  date steps
    ## 2012-10-01 2012-10-01     0
    ## 2012-10-02 2012-10-02   126
    ## 2012-10-03 2012-10-03 11352
    ## 2012-10-04 2012-10-04 12116
    ## 2012-10-05 2012-10-05 13294
    ## 2012-10-06 2012-10-06 15420

1.  Make a histogram of the total number of steps taken each day.

``` r
ggplot(df, aes(date, steps)) + geom_bar(stat = "Identity", fill = 'salmon', na.rm = TRUE)+labs(title ="Total steps taken per day")
```

![](Report1_files/figure-markdown_github/unnamed-chunk-3-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

``` r
# Mean of total steps per day
mean(df$steps)
```

    ## [1] 9354.23

``` r
# Median of total steps per day
median(df$steps)
```

    ## [1] 10395

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

``` r
time_series <- tapply(Activity$steps, Activity$interval, mean, na.rm = TRUE)
plot(row.names(time_series), time_series, type = "l", xlab = "5-min interval", 
    ylab = "Average across all Days", main = "Average number of steps taken", 
    col = "red")
```

![](Report1_files/figure-markdown_github/unnamed-chunk-6-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

``` r
max_interval <- which.max(time_series)
names(max_interval)
```

    ## [1] "835"

### Imputing missing values

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

``` r
sum(is.na(Activity$steps))
```

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

``` r
# We will fill the missing values in using the median

StepsAverage <- aggregate(steps ~ interval, data = Activity, FUN = median)
fillNA <- numeric()
for (i in 1:nrow(Activity)) {
    obs <- Activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

``` r
new_activity <- Activity
new_activity$steps <- fillNA
```

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

``` r
sumTable <- rowsum(new_activity$steps, group = new_activity$date, na.rm =TRUE )
df <- data.frame(cbind(row.names(sumTable), sumTable))
names(df) <- c("date", "steps") 
df$steps <- as.numeric(levels(df$steps))[df$steps]

ggplot(df, aes(date, steps)) + geom_bar(stat = "Identity", fill = 'salmon', na.rm = TRUE)+labs(title ="Total steps taken per day")
```

![](Report1_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
mean(df$steps) 
```

    ## [1] 9503.869

``` r
median(df$steps)
```

    ## [1] 10395

### Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

``` r
Activity$date <- as.Date(Activity$date, "%Y-%m-%d")
day <- weekdays(Activity$date)
daylevel <- vector()
for (i in 1:nrow(Activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
Activity$daylevel <- daylevel
Activity$daylevel <- factor(Activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = Activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")

head(Activity)
```

    ##   steps       date interval daylevel
    ## 1    NA 2012-10-01        0  Weekday
    ## 2    NA 2012-10-01        5  Weekday
    ## 3    NA 2012-10-01       10  Weekday
    ## 4    NA 2012-10-01       15  Weekday
    ## 5    NA 2012-10-01       20  Weekday
    ## 6    NA 2012-10-01       25  Weekday

1.  Make a panel plot containing a time series plot (i.e. type = “l”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

``` r
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](Report1_files/figure-markdown_github/unnamed-chunk-14-1.png)

-fin
----
