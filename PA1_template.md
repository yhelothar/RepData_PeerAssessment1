---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---
## Loading and preprocessing the data

```r
dat<-read.csv('activity.csv')
```
## What is mean total number of steps taken per day?

```r
stepsPerDay<-sapply(split(dat$steps,dat$date),sum)
hist(stepsPerDay)
```

![plot of chunk plot](figure/plot-1.png) 

```r
meanSteps<-mean(stepsPerDay[!is.na(stepsPerDay)])
medianSteps<-median(stepsPerDay[!is.na(stepsPerDay)])
```
The mean steps per day is 1.0766189 &times; 10<sup>4</sup> and the median steps per day is 10765.  

## What is the average daily activity pattern?

```r
dailyMeanActivity<-tapply(dat$step,dat$interval,mean,na.rm=TRUE)
intervals=levels(factor(dat$interval))
plot(intervals,dailyMeanActivity,type="l",main='Daily Mean Activity')
```

![plot of chunk plotmean](figure/plotmean-1.png) 

```r
maxInd<-match(max(dailyMeanActivity),dailyMeanActivity)
maxInterval<-intervals[maxInd]
```
The 5 minute interval containing the maximum number of steps is at 835  
## Inputing missing values

```r
numNAs<-length(dat[is.na(dat$steps)==TRUE,'steps'])
datFilled<-dat
dailyMeanVec<-rep(dailyMeanActivity,length=length(dat$steps))
for(i in 1:length(dat$steps)){
        if(is.na(datFilled[i,'steps'])==TRUE){
                datFilled[i,'steps']<-dailyMeanVec[i]
        }
}

stepsPerDayFilled<-sapply(split(datFilled$steps,datFilled$date),sum)
hist(stepsPerDayFilled)
```

![plot of chunk histplot](figure/histplot-1.png) 

```r
meanStepsFill<-mean(stepsPerDayFilled[!is.na(stepsPerDayFilled)])
medianStepsFill<-median(stepsPerDayFilled[!is.na(stepsPerDayFilled)])
meanStepsDiff<-abs(meanStepsFill-meanSteps)
medianStepsDiff<-abs(medianStepsFill-medianSteps)
```
Mean steps taken per day is 1.0766189 &times; 10<sup>4</sup>. Median steps taken per day is 1.0766189 &times; 10<sup>4</sup>.  
These two have a difference of 0 and 1.1886792 from the nonfilled mean and median, respectively. These two differences are negligible and thus the values do not differ.  

## Are there differences in activity patterns between weekdays and weekends?

```r
week<-sapply(as.Date(dat$date),weekdays)
isWeekend<-rep(c('Weekday','Weekend'),length=length(week))
for(i in 1:length(week)){
        if(week[i]=='Monday'| week[i]=='Tuesday'| week[i]=='Wednesday'| week[i]=='Thursday'| week[i]=='Friday'){
                isWeekend[i]<-'Weekday'
                } else{
                        isWeekend[i]<-'Weekend'
                }
}
dat<-data.frame(dat,isWeekend)
weekdayMeanActivity<-tapply(dat[dat['isWeekend']=='Weekday','steps'],dat[dat['isWeekend']=='Weekday','interval'],mean,na.rm=TRUE)
weekendMeanActivity<-tapply(dat[dat['isWeekend']=='Weekend','steps'],dat[dat['isWeekend']=='Weekend','interval'],mean,na.rm=TRUE)

par(mfrow=c(2,1))
plot(intervals,weekdayMeanActivity,type='l',main='Weekday')
plot(intervals,weekendMeanActivity,type='l',main='Weekend')
```

![plot of chunk plotcomparison](figure/plotcomparison-1.png) 
