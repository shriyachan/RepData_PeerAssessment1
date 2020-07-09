---
title: "coureseraproj1"
author: "shriya"
date: "09/07/2020"
output: html_document
---
# Solution
##### downloading required libraries
```{r echo=TRUE}
library(knitr)
library(ggplot2)
library(data.table)
opts_chunk$set(echo = TRUE, results = 'hold')
```
# Loading the data
```{r echo =TRUE}
act_data <- read.csv("C:/Users/Murali/Downloads/repdata_data_activity/activity.csv", header=TRUE, sep=",")
str(act_data)
```
# Processing
#### Convert some of the vectors to appropriate forms
```{r echo=TRUE}
act_data$date <- as.Date(act_data$date, format="%Y-%m-%d")
act_data$interval <- as.factor(act_data$interval)
```
#### Post converting the column classes print the structure of the data
```{r echo=TRUE}
str(act_data)
```
#### Print the header of the dataset
```{r echo=TRUE}
head(act_data, 10)
```
# What is mean total number of steps taken per day?
## 1. Total number of steps taken per day
```{r echo=TRUE}
steps_per_day <- aggregate(steps ~ date, data=act_data, FUN=sum)
colnames(steps_per_day) <- c("date", "steps")
```
##### printing headers with avg steps per day
```{r echo=TRUE}
head(steps_per_day, 10)
```
## 2. Make a histogram of the total number of steps taken each day
```{r echo= TRUE}
ggplot(steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title = "Histogram - Steps Taken Per Day", x = "Steps Per Day", y = "Frequency")
```
## 3. Calculate and report the mean and median of the total number of steps taken per day
``` {r echo=TRUE}
mean_steps_per_day <- mean(steps_per_day$steps)
mean_steps_per_day
median_steps_per_day <- median(steps_per_day$steps)
median_steps_per_day
```
# What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
``` {r echo=TRUE}
steps_per_interval <- aggregate(steps ~ interval, data = act_data, FUN = mean, na.rm = TRUE)
steps_per_interval$interval <- as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")
```
#### printing header
``` {r echo=TRUE}
head(steps_per_interval, 10)
```
#### Plot the timeseries graph
```{r echo=TRUE}

ggplot(steps_per_interval, aes(x = interval, y = steps)) + 
  geom_line(col = "blue", size = 1) + 
  labs(title = "Average Daily Activity Pattern", x = "Interval", y = "Steps")

```
## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
``` {r echo= TRUE}
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]
max_interval
```
# Inputing missing values
## 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
``` {r echo=TRUE}
missing_values <- sum(is.na(act_data$steps))
missing_values
```
## 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.
## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
``` {r echo=TRUE}
new_act_data <- act_data
index_of_na <- which(is.na(new_act_data$steps))
for (i in index_of_na) {
  new_act_data$steps[i] <- with(steps_per_interval, steps[interval = new_act_data$interval[i]])
}
```
#### printing header
``` {r echo=TRUE}
head(new_act_data, 10)
```
#### checking if missing values are there
``` {r echo=TRUE}
new_missing_values <- sum(is.na(new_act_data$steps))
new_missing_values
```
## 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
``` {r echo=TRUE}
new_steps_per_day <- aggregate(steps ~ date, data = new_act_data, FUN=sum)
colnames(new_steps_per_day) <- c("date", "steps")
ggplot(new_steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title = "Histogram - Steps Taken Per Day", x = "Steps Per Day", y = "Frequency")
```
#### computing mean median
```{r echo= TRUE}
new_mean_steps_per_day <- mean(new_steps_per_day$steps)
new_mean_steps_per_day
new_median_steps_per_day <- median(new_steps_per_day$steps)
new_median_steps_per_day
```
# Are there differences in activity patterns between weekdays and weekends?
## 1. Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
#### Let us first add a factor variable to identify the given date as Weekday or Weekend
```{r echo=TRUE}
dt <- data.table(new_act_data)
dt[, weekday := ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")]
dt$weekday <- as.factor(dt$weekday)
dt$interval <- as.integer(levels(dt$interval)[dt$interval])
head(dt, 10)
```
## 2. Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
``` {r echo=TRUE}
steps_per_weekday <- aggregate(steps ~ interval+weekday, data = dt, FUN = mean)
ggplot(steps_per_weekday, aes(x = interval, y = steps)) + 
  geom_line(col = "blue", size = 1) + 
  facet_wrap(~ weekday, nrow=2, ncol=1) + 
  labs(x = "Interval", y = "Number of Steps")
```
### Conclusion
#### Looking at the above graph we notice that the activity on weekdays has the highest peak (> 300) compared to all intervals and only one other peak that touches 100. On the contrary, weekend intervals have more peaks over a hundred than weekday. May be the person from whomever the data is collected is engaged in more active life style during weekends compared to weekdays.
