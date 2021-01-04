#extracting and loading the data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","./activity.zip")
activity <- unzip("./activity.zip")
r_activity <- read.csv("./activity.csv")
rm(activity)
library(dplyr)

#defining a subset without NA in steps variable
data <- na.omit(r_activity) 

#data pre processing
data$date <- as.Date(data$date)
data$steps <- as.numeric(as.character(data$steps))

#obtaining the aggregated indicators for steps
steps_ind <- data %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

#total steps histogram
hist(steps_ind$total_steps,col = 4,xlab = "Total steps per day",ylab = "Count",main = "Steps per day Histogram")

#mean and median values
mean_steps <- mean(steps_ind$total_steps)
median_steps <- median(steps_ind$total_steps)

#defining 5-minute intervals time series
#grouping and calculating steps in the interval
fivemin <- data %>%
    group_by(interval) %>%
    summarize(avg_steps = mean(steps))

#plotting time series
plot(fivemin$interval,fivemin$avg_steps,type="l",lwd = 2,col = 4, xlab = "Interval",ylab = "Average Steps",main = "Average steps in 5-minute interval")

max_steps_int <- fivemin$interval[which.max(fivemin$avg_steps)]


#calculating number NA values
missing <- sum(is.na(r_activity$steps))

#filling missing values with data from the 5-minute interval data
totaldata <- r_activity
    for (x in 1:nrow(totaldata)) {
        if (is.na(totaldata$steps[x])) {
            index_s <- which(totaldata$interval[x] == fivemin$interval)
            totaldata$steps[x] <- fivemin[index_s,]$avg_steps
        }
        
    }
totaldata$date <- as.Date(totaldata$date)


#calculating number NA values of the filled dataset
missing <- sum(is.na(totaldata$steps))

#histogram of missing data filled
totaldata_steps <- totaldata %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps))
hist(totaldata_steps$total_steps,col = 5,xlab = "Total steps per day",ylab = "Count",main = "Steps per day Histogram")

filldatamean <- mean(totaldata_steps$total_steps)
filldatamedian <- median(totaldata_steps$total_steps)

#generate weekdays variable
totaldata$day <- weekdays(totaldata$date)

#define weekday
totaldata$day_type <- "weekday"

#override for weekend days
#in Chile the language is set in spanish, that's the reason for the following codeline
totaldata$day_type[totaldata$day %in% c("sábado","domingo")] <- "weekend"

#average steps per day type
avg_days <- totaldata %>%
    group_by(day_type,interval) %>%
    summarize(avg_steps = mean(steps))

#panel plot weekday vs weekend
library(ggplot2)
qplot(interval,avg_steps,data = avg_days,
      geom =  "line",
      xlab = "Interval",
      ylab = "Average steps",
      main = "Avg steps Weekdays vs Weekends",
      facets = day_type~.)