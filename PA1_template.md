---
title: "Steps Monitoring Data"
author: "Symphony"
---

## Loading and Preprocessing Data
  
```{r}
data<-read.csv("activity.csv")
str(data)
data$date<-as.character(data$date)
data$date<-as.Date(data$date)
str(data)

```
  
## What is mean total number of steps taken per day?
  
```{r}
daysteps<-aggregate(data$steps,by=list(data$date),FUN=sum,na.rm=TRUE)
names(daysteps)<-c("date","totalsteps")
hist(daysteps$totalsteps,breaks = 10,col="green",main="Histogram of total number of steps taken each day",xlab="Number of Steps")
mean<-mean(daysteps$totalsteps)
median<-quantile(x = daysteps$totalsteps,probs = c(0.5))
mean
median
```
  
  The mean  = 9354.23 and median = 10395
  
## What is the average daily activity pattern?
  
```{r}
intervalsteps<-aggregate(data$steps,by=list(data$interval),FUN=mean, na.rm=TRUE)
str(intervalsteps)
names(intervalsteps)<-c("interval","averagesteps")
plot(x = intervalsteps$interval,y=intervalsteps$averagesteps,type = "l",main="Time series of average steps",xlab ="5 minutes interval", ylab="Average steps" )
intervalsteps[intervalsteps$averagesteps==max(intervalsteps$averagesteps),]
```
 
 The time interval 0835 contains the maximum number of steps with value 206.1698, on average across all the days in the dataset.
 
## Imputing missing values

```{r}
table(is.na(data$steps))

# replace missing values with average steps for each time interval across all the days
newdata<-data
newdata$steps[is.na(data$steps)]<-intervalsteps$averagesteps[match(data$interval[is.na(data$steps)],table=intervalsteps$interval)]
table(is.na(newdata$steps)) #newdata is a new dataset that is equal to the original dataset but with the missing data filled in

newdaysteps<-aggregate(newdata$steps,by=list(newdata$date),FUN=sum,na.rm=TRUE)
names(newdaysteps)<-c("date","totalsteps")
hist(newdaysteps$totalsteps,breaks = 10,col="green",main="Histogram of total number of steps taken each day for new data set",xlab="Number of Steps")
newmean<-mean(newdaysteps$totalsteps)
newmedian<-quantile(x = newdaysteps$totalsteps,probs = c(0.5))
newmean
newmedian
diffmean<-mean-newmean
diffmedian<-median-newmedian
diffmean
diffmedian


```
  
  There are 2304 missing values (number of rows with NA value for steps). I replace the missing values with average steps for each time interval across all the days.
  
  For new data set with missing values replaced, mean = 10766.19 and median = 10766.19
  
  The difference between the two means = 1411.959 in value and between two medians = 371.1887 in value.
  
  The impact of replacing missing data increased  the value of mean and median and the histogram looks more like a bell curve/ Gaussian distributed.
  
## Are there differences in activity patterns between weekdays and weekends?
  
```{r}
newdata$daytype<-weekdays(newdata$date)
head(newdata)
newdata$type<-"weekday"
newdata$type[is.element(newdata$daytype,set = c("Saturday","Sunday"))]<-"weekend"
newdata$type<-as.factor(newdata$type)
str(newdata)

newintervalsteps<-aggregate(newdata$steps,by=list(newdata$type,newdata$interval),FUN=mean, na.rm=TRUE)
head(newintervalsteps)
names(newintervalsteps)<-c("type","interval","averagesteps")
library(lattice)
xyplot(averagesteps ~ interval | type,data=newintervalsteps,layout=c(1,2), type="l",xlab="5 minutes interval",ylab="Average number of steps", main = "Comparisons by weekday and weekend")
```
  
  Notice that the maximum average steps during weekday is larger than that during weekend. Notice that during weekend, the sample walks more steps for most of the time interval compared to that during weekday.