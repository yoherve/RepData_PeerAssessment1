###Reproducible Research - Peer Assessment 1
####Kyle Statham - *July 2015*  

This assignment makes use of data from a personal activity monitory device. The data consists of two months of measurements from an anonymous individual collected during October and November, 2012 and includes the number of steps taken, measured at 5 minute intervals each day.

Summary information on total, mean and median steps per day, along with average daily activity pattern will be presented. The impact of imputing missing values and differences between weekday and weekend activity will also be examined.

The following code assumes the working directory in R has been set with the setwd() function and the required activity.csv file is present in that working directory. 

The dataset can be found here: [Activity monitoring data [52k]] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)  


####1. Load the data from "activity.csv"" into data frame "activity"

```{r load data set}
# Load "activity.csv" into data frame "activity"
activity <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
# Check results
str(activity)
head(activity)
```


####2. Process/transform the data as necessary into a format suitable for analysis

```{r process/transform data}
# Transform date variable to date
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
# Transform interval variable to factor
activity$interval <- as.factor(activity$interval)
# Create data frame activity_nona without NA's
activity_nona <- na.omit(activity)
# Set options
options(digits = 10, scipen = 3, tidy = FALSE)
# Check results
str(activity_nona)
head(activity_nona)
```


####3. Ignoring missing values, calculate the total number of steps taken per day, make a histogram of total number of steps taken per day and calculate the mean and median total number of steps taken per day

```{r calculate total steps per day}
# Calculate total steps per day
StepsPerDay <- aggregate(activity_nona$steps, list(date=activity_nona$date), sum, na.rm = TRUE)
colnames(StepsPerDay) <- c("date","total_steps")
# Check results
str(StepsPerDay)
head(StepsPerDay)
```

Create a histogram showing total number of steps per day

```{r create steps per day histogram}
#Create histogram
hist(StepsPerDay$total_steps, breaks = 25, col = "red",
     xlab = "Total Number of Steps per Day", 
     main = "Total Number of Steps per Day (NA values ignored)")
```

Calculate the mean and median number of steps per day

```{r calculate mean and median steps per day}
# Calculate mean and median
StepsPerDayMean <- round(mean(StepsPerDay$total_steps, na.rm = TRUE),0)
StepsPerDayMedian <- (median(StepsPerDay$total_steps, na.rm = TRUE))
```

####==========================================================================
####>Ignoring NA values gives a mean of `r StepsPerDayMean` and a median of `r StepsPerDayMedian`.
####==========================================================================

####4. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days and identify which 5-minute interval contains the maximum number of steps

```{r calculate mean interval activity}
# Calculate interval means
IntervalMeans <- aggregate(activity_nona$steps, list(date=activity_nona$interval), mean, na.rm = TRUE)
colnames(IntervalMeans) <- c("interval","mean")
# Check results
str(IntervalMeans)
head(IntervalMeans) 
```

Create the time series plot

```{r create interval means time series plot}
plot(IntervalMeans$interval, IntervalMeans$mean, type="l", col = "blue", 
     xlab="Interval (minutes)", ylab="Average Number of Steps",
     main="Time Series of Average Steps per Interval (NA values ignored)")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r calculate interval with max average steps}
#Find the highest mean
MaxMean <- which(IntervalMeans$mean == max(IntervalMeans$mean))
#Show the interval associated with the highest mean
MaxInterval <- IntervalMeans[MaxMean, 1]
```

####==========================================================================
####>The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is `r MaxInterval`.
####==========================================================================

####5. Calculate and report the total number of missing values in the dataset, create a new dataset by replacing the missing values with corresponding interval means, then using this new dataset make a histogram of total steps taken each day and calculate the mean and median of total steps per day. Finally, report on differences between total/mean/median steps per day between what this new dataset shows and what was calculated for the dataset which ignored NA values.

```{r calculate total number of missing values}
NAcount <- sum(!complete.cases(activity))
```

####==========================================================================
####>The total number of incomplete cases is `r NAcount`.
####==========================================================================

Create a new dataset "activity_imputed" where NA values for the step variable are replaced with the corresponding interval mean

```{r replace step NAs with associated 5-minute interval mean}
activity_imputed <- activity 
for (i in 1:nrow(activity_imputed)) {
        if (is.na(activity_imputed$steps[i])) {
                activity_imputed$steps[i] <- IntervalMeans[which(activity_imputed$interval[i] == IntervalMeans$interval), ]$mean
        }
}
```

Calculate the total steps per day using imputed values

```{r calculate total steps per day with imputed values}
# Calculate total steps per day
StepsPerDayImputed <- aggregate(activity_imputed$steps, list(date=activity_imputed$date), sum, na.rm = TRUE)
colnames(StepsPerDayImputed) <- c("date","total_steps")
# Check results
str(StepsPerDayImputed)
head(StepsPerDayImputed)
```

Create a histogram showing total number of steps per day using imputed values

```{r create imputed steps per day histogram}
#Create histogram
hist(StepsPerDayImputed$total_steps, breaks = 25, col = "green", 
     xlab = "Total Number of Steps per Day", 
     main = "Total Number of Steps per Day (NA values imputed)")
```

Calculate the imputed mean and median number of steps per day

```{r calculate imputed mean and median steps per day}
# Calculate mean and median
StepsPerDayMeanImputed <- round(mean(StepsPerDayImputed$total_steps, na.rm = TRUE),0)
StepsPerDayMedianImputed <- round(median(StepsPerDayImputed$total_steps, na.rm = TRUE),0)
```

####==========================================================================
####>Replacing NA values with corresponding interval means gives a mean of `r StepsPerDayMeanImputed` and a median of `r StepsPerDayMedianImputed`.  
####==========================================================================


####6. Using the dataset containing imputed values, create a new variable in the dataset to denote whether the activity reading occurred on a weekday or a weekend. Make a panel plot containing a time series plot of average steps per 5-minute interval, averaged across all weekday days or weekend days.

```{r create day of the week variable}
activity_imputed$day <- factor(format(activity_imputed$date, "%A"))
levels(activity_imputed$day)
levels(activity_imputed$day) <- list(weekday = c("Monday", "Tuesday", 
                                                 "Wednesday", "Thursday", 
                                                 "Friday"), 
                                     weekend = c("Saturday", "Sunday"))
levels((activity_imputed$day))
```

Calculate new averages by weekday and weekend

```{r calculate weekday means}
DayMeans <- aggregate(activity_imputed$steps, list(interval = as.numeric(as.character(activity_imputed$interval)), weekdays = activity_imputed$day), FUN = "mean")
names(DayMeans)[3] <- "DayStepMeans"
```

Create panel plot to compare weekend vs weekday activity

```{r create weekday vs weekend panel plot}
library(lattice)
weekdayplot <- xyplot(DayMeans$DayStepMeans ~ DayMeans$interval | DayMeans$weekdays, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
print(weekdayplot)
```
