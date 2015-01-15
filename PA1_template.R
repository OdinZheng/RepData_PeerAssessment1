setwd("C:/Users/Odin/Documents/GitHub/RepData_PeerAssessment1")

## Loading and preprocessing the data
# read the data from csv file
act_data <- read.csv("activity.csv", header = TRUE)
library(plyr)
library(ggplot2)

## What is mean total number of steps taken per day?
# ignore the missing values

# histogram

ggplot(act_data, aes(x = act_data$date, y = act_data$step)) + geom_bar(stat = "identity")

#total_step_perday = ddply(act_data, .(date), function(df) sum(df$step, na.rm = TRUE))
#ggplot(act_data, aes(x = total_step_perday$date, y = total_step_perday$V1)) + geom_bar(stat = "identity")

# compute mean and meadian of steps per day
#mean_step_perday = ddply(act_data, .(date), function(df) mean = mean(df$step, na.rm = TRUE))
#med_step_perday = ddply(act_data, .(date), function(df) median = median(df$step, na.rm = TRUE))

#ggplot(mean_step_perday, aes(x = date, y = V1)) + geom_bar(stat = "identity")
#ggplot(med_step_perday, aes(x = date, y = V1)) + geom_bar(stat = "identity")

mean_step_perday <- mean(act_data$step, na.rm = TRUE)
med_step_perday <- median(act_data$step, na.rm = TRUE)

## What is the average daily activity pattern
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)

step_vs_interval = ddply(act_data, .(interval), function(df) mean = mean(df$step, na.rm = TRUE))
names(step_vs_interval)[2] = "mean_steps";

ggplot(step_vs_interval, aes(x = interval, y = mean_steps)) + geom_bar(stat = "identity")

max_index <- which.max(step_vs_interval$mean_steps)
max_index
max_interval <- step_vs_interval[max_index,1]
max_interval
# the maximum mean steps is 206.1698 at the 104th interval (835)

## Imputing missing values; missing values only exist in "steps"
miss_num <- sum(is.na(act_data[,1]))
miss_num # 2304

# duplicate the original dataset, and then
# fill the NAs with mean of the current interval

act_data_2 <- act_data

miss_row_index <- which(is.na(act_data_2[,1]))
for (i in 1:miss_num)
{
    interval_value <- act_data_2[miss_row_index[i],3]
    
    act_data_2[miss_row_index[i],1] <- step_vs_interval$mean_steps[step_vs_interval$interval == interval_value]
}

# miss_row_index <- which(is.na(act_data[,1]))

ggplot(act_data_2, aes(x = act_data_2$date, y = act_data_2$step)) + geom_bar(stat = "identity")

mean(act_data_2$steps)
mean(act_data$steps, na.rm = TRUE)

median(act_data_2$steps)
median(act_data$steps, na.rm = TRUE)

# The impulation of the missing values does not effect the final
# results to much (37.3826/0)

## Are there differences in activity patterns between weekdays and weekends?
# use act_data_2 for this part
Sys.setlocale(,"C")
act_data_2 <- transform(act_data_2, day_type = weekdays(as.Date(act_data_2$date,"%Y-%m-%d")))
# if else: 
act_data_2$day_type <- sapply(act_data_2$day_type, 
                             function(x) ifelse(as.character(x) == "Sunday" | as.character(x) == "Saturday",
                                                act_data_2$day_type <- 2, act_data_2$day_type <- 1))

act_data_2$day_type<- factor(act_data_2$day_type, labels = c("weekday", "weekend"))

str(act_data_2)


step_vs_interval_weekday = ddply(subset(act_data_2,day_type == "weekday"), .(interval), function(df) mean = mean(df$step, na.rm = TRUE))
names(step_vs_interval_weekday)[2] = "mean_steps_weekday";
p1 <- ggplot(step_vs_interval_weekday, aes(x = interval, y = mean_steps_weekday)) + geom_bar(stat = "identity") + ggtitle("Weekday Steps")

step_vs_interval_weekend = ddply(subset(act_data_2,day_type == "weekend"), .(interval), function(df) mean = mean(df$step, na.rm = TRUE))
names(step_vs_interval_weekend)[2] = "mean_steps_weekend";
p2 <- ggplot(step_vs_interval_weekend, aes(x = interval, y = mean_steps_weekend)) + geom_bar(stat = "identity") + ggtitle("Weekend Steps")
source("multiplot.r")
multiplot(p1, p2, cols=1)





