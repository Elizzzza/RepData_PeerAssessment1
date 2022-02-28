library(tidyverse)

### Loading the data
activity <- unzip("activity.zip")
act <- read.csv("activity.csv", header=TRUE, sep=",")
str(act)

### Processing the data
act <- act %>% 
        mutate(date = as.Date(date, format="%Y-%m-%d"))
head(act)     ### Print the header of the dataset
str(act)

### calculate the sums of steps per day
act1 <- act %>% 
        group_by(date) %>% 
        summarise(steps_per_day = sum(steps)) %>% 
        mutate(steps_per_day = as.numeric(steps_per_day))

### Make a histogram of the total number of steps taken each day
ggplot(act1, aes(x=steps_per_day)) + 
        geom_histogram(col = "blue") + 
        labs(x = "Steps Per Day", y = "Frequency") +
        ggtitle("Histogram of Steps Taken Per Day") +
        theme_bw()

### calculate the mean of sums of steps per day
act1 %>% select(steps_per_day) %>% 
        colMeans(na.rm = TRUE)

### calculate the median of sums of steps per day
median(act1$steps_per_day, na.rm = TRUE)


### calculate the mean of steps per day
act2 <- act %>% 
        group_by(interval) %>% 
        summarise(mean_steps = mean(steps, na.rm = TRUE)) 
head(act2)  ### Print the header of the dataset


### Make a time series plot of the 5-minute interval (x-axis) 
### and the average number of steps taken, averaged across all days (y-axis)
ggplot(act2, aes(x = interval, y = mean_steps)) + 
        geom_line(col = "blue", size = 1) + 
        labs(title = "Average Daily Activity Pattern", x = "5-minute Interval", 
             y = "Average Steps across all days") +
        theme_bw()

### Find max steps in all 5-min interval
max_interval <- act2[which.max(act2$mean_steps),]
max_interval
### Alternative way to find max steps in all 5-min interval
max_steps <- max(act2$mean_steps)
act2 %>%
        filter(mean_steps == max_steps)

### Calculate and report the total number of missing values in the dataset 
### (i.e. the total number of rows with *NA*s)
is_complete <- complete.cases(act)
sum(!is_complete)

### Devise a strategy for filling in all of the missing values in the dataset
### Impute with mean steps for that day
avg_interval <- act %>%
        group_by(interval) %>%
        summarise(avg_steps = mean(steps, na.rm = TRUE))

### Create a new dataset that is equal to the original dataset with the missing data filled in
act_imputed <- act
    for (i in 1:nrow(act_imputed)) {
            if (is.na(act_imputed[i, "steps"]) == TRUE) {
                   data_interval <- act_imputed[i, "interval"]
                   imputed_value <- avg_interval[avg_interval$interval == data_interval, "avg_steps"]
                   act_imputed[i, "steps"] <- imputed_value
            } else {
                act_imputed[i, "steps"] <- act_imputed[i, "steps"]
            }
    }
### check number of rows with NA in imputed dataset
is_complete <- complete.cases(act_imputed)
sum(!is_complete)


### calculate the total number of steps per day with imputed dataset
act3 <- act_imputed %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps, na.rm = TRUE))
### Make a histogram of the total number of steps taken each day
ggplot(act3, aes(x = total_steps)) +
    geom_histogram(col = "blue") + 
    labs(x = "Steps Per Day", y = "Frequency") +
    ggtitle("Histogram of Total Steps Taken Per Day") +
    theme_bw()
    
### Calculate and report the mean and median total number of steps taken per day
mean(act3$total_steps)
median(act3$total_steps)

### Create a new factor variable in the dataset with two levels 
### “weekday” and “weekend” indicating whether a given date is a weekday or weekend day
act_imputed <- act_imputed %>% 
    mutate(weekday = ifelse(weekdays(date) == c("Saturday", "Sunday"), "weekend", "weekday"))
           
### Alternative way to add col:weekday in act_imputed
act_imputed$weekday <- ifelse(weekdays(act_imputed$date) %in% 
                                    c("Saturday", "Sunday"), "weekend", "weekday")

### Make a panel plot containing a time series plot of the 5-minute interval (x-axis) 
### and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
act4 <- act_imputed %>%
    group_by(interval, weekday) %>%
    summarise(avg_steps = mean(steps, na.rm = TRUE))

ggplot(act4, aes(x = interval, y = avg_steps)) +
    geom_line(color = "blue") +
    facet_wrap(~weekday, nrow=2) +    ### Divide by weekday, going horizontally
    labs(x = "5-minute Interval", 
         y = "Average Number of Steps \n across all weekdays or weekends") +
    theme_bw()



