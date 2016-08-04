library(dplyr)
library(ggplot2)

activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
act <- tbl_df(activity)
str(act)
nas <- is.na(act$steps)
sum(nas)
nrow(act)
act$date <- as.Date(act$date)

# USELESS calculate total steps per day
# steps <- act %>% group_by(date) %>% summarise(total.steps=sum(steps))
steps <- act %>% group_by(date) %>%
    filter(steps>0) %>%
    summarise(total.steps=sum(steps), average.steps=mean(steps), median.steps=median(steps))

# https://github.com/cardiomoon/RepData_PeerAssessment1/blob/master/PA1_template.Rmd
# > totalSteps<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
# > mean(totalSteps$steps)
# [1] 10766.19
# > median(totalSteps$steps)
# [1] 10765

# groups for all intervals
int <- group_by(act, interval)
# creates averages
avg <- summarise(int, avg=mean(steps, na.rm = TRUE))
# plot(avg$avg~avg$interval, type="l")
# plot(avg$interval, avg$avg, type="l")
# plots
plot(avg$interval, avg$avg, type="l", xlab="Interval", ylab="Average steps")

# missing values
sum(is.na(act$steps))
# get avg of single days
avg <- summarise(int, avg=mean(steps, na.rm = TRUE))
# get avg of specific day
avg[avg$date=="2012-10-07",]


# missing values
library(mice)
library(VIM)

md.pattern(activity)
aggr_plot <- aggr(activity, col=c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, labels=names(activity), cex.axis=.7, gap=3)

tempData <- mice(activity,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
head(temp$imp$steps)
complData <- complete(temp, 1)
# cross check
sum(is.na(activity$steps))
sum(is.na(complData$steps))



activity_complete$date <- as.Date(activity_complete$date)
activity_complete$daytype <- factor(
    (weekdays(activity_complete$date) %in% weekdays1), 
    levels=c(FALSE, TRUE), 
    labels=c('weekend', 'weekday'))
mean_steps_daytype  <- aggregate(steps~interval+daytype,activity_complete,mean)
ggplot(mean_steps_daytype, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .)
