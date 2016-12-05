

```r
---
title: "PA1_template.Rmd"
author: "Andrew"
date: "9/11/2016"
output: html_document
---

###Assignment
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.
```


##Loading and preprocessing the data

```r
Data<-read.csv("E:/Dropbox/Dropbox/Data Science Courses/Data Science Specialization/05 - Reproducible Research/02 - Markdown and knitr/repdata%2Fdata%2Factivity/activity.csv")

str(Data)

Data$date <- as.Date(Data$date, format = "%Y-%m-%d")

str(Data)
```

##What is mean total number of steps taken per day?

```r
hist(tapply(Data$steps,Data$date,sum),breaks=10,xlab="Number of Steps",main="Total Number of Steps Per Day")
meansteps<-as.integer(mean(tapply(Data$steps,Data$date,sum), na.rm=TRUE))
mediansteps<-median(tapply(Data$steps,Data$date,sum), na.rm=TRUE)
```
The mean number of steps taken per day is 10766.
The median number of steps taken per day is 10765.

##What is the average daily activity pattern?

```r
avgstepsbyinterval <- aggregate(steps ~ interval, Data, mean,na.rm=TRUE)
library(ggplot2)
ggplot(avgstepsbyinterval,aes(x=interval,y=steps,group=0)) +
    geom_line(size=1) + 
    labs(title="Avg Number of Steps by 5 min. Interval",x="Interval",y="Avg # of Steps")

maxsteps <- round(max(avgstepsbyinterval$steps),digits=0)
intervalmax <- avgstepsbyinterval[which.max(avgstepsbyinterval$steps),]
```
The maximum average number of steps of 206 occurs at interval 835.

##Imputing missing values

```r
summary(Data)
NAs<-sum(is.na(Data$steps))
```
There are 2304 NA step values.


```r
imputeData <- mice(Data[-2], m=5,method="pmm", maxit=50, seed=200, print=FALSE)
imputedSteps <- complete(imputeData,1) 
summary(imputedSteps)


hist(tapply(imputedSteps$steps,Data$date,sum),breaks=10,xlab="Number of Steps",main="Total Number of Steps Per Day")
meansteps<-as.integer(mean(tapply(imputedSteps$steps,Data$date,sum), na.rm=TRUE))
mediansteps<-median(tapply(imputedSteps$steps,Data$date,sum), na.rm=TRUE)
```
The mean number of steps taken per day is 10766.
The median number of steps taken per day is 10765.
The imputation of missing steps values has increased the average steps.

##Are there differences in activity patterns between weekdays and weekends?

```r
Data$day_type <- chron::is.weekend(Data$date)
imputedSteps$day_type <- as.factor(Data$day_type)
levels(imputedSteps$day_type)=c("weekday","weekend")

forGraph <- aggregate(steps ~ interval+day_type, imputedSteps, mean,na.rm=TRUE)

ggplot(forGraph, aes(x=interval, y=steps,color=day_type)) + 
        geom_line() +
        facet_wrap(~day_type, nrow=2, ncol=1) +
        ggtitle("Avg # of Steps - Weekday vs. Weekend")+
        labs(x="Interval", y="Avg # of Steps") +
        theme_bw()+theme(legend.position="none") 
```
```

