
## Loading and preprocessing the data

data <- read.csv("C:/Users/User/Desktop/repdata_data_activity/activity.csv")

##What is mean total number of steps taken per day?

library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
qplot(total.steps, binwidth = 500, xlab = "Total number of steps taken each day")

mean(total.steps, na.rm = TRUE)
median(total.steps, na.rm = TRUE)

##What is the average daily activity pattern?
library(ggplot2)
averages <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), 
                      FUN = mean, na.rm = TRUE)
ggplot(data = averages, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
  ylab("Average number of steps taken")

averages[which.max(averages$steps), ]

missing <- is.na(data$steps)
# How many missing
table(missing)


fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps)) 
    filled <- c(steps) else filled <- (averages[averages$interval == interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

total.steps <- tapply(filled.data$steps, filled.data$date, FUN = sum)
qplot(total.steps, binwidth = 500, xlab = "Total number of steps taken each day")

mean(total.steps)
median(total.steps)

filled.data$date<- as.Date(filled.data$date, "%Y-%m-%d")
filled.data$weekend <- as.factor(ifelse(weekdays(filled.data$date)=="Σάββατο" | weekdays(filled.data$date)=="Κυριακή","weekend","weekday"))
filled.data$dayofweek <- as.factor(weekdays(filled.data$date))

averages <- aggregate(steps ~ interval + weekend, data = filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .) + 
  xlab("5-minute interval") + ylab("Number of steps")
View(data)
