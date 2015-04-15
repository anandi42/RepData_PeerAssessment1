---
title: "Reproducible Research Peer Assesment 1"
output:
  html_document:
    fig_caption: yes
    fig_height: 4.5
    fig_width: 6.5
    keep_md: yes
    self_contained: yes    
---

##Introduction
Personal activity monitoring devices allow users to collect a large amount of data about themselves. In this report, we will analyze of set of such data.  
The data used in this report was obtained [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) (link is to zip file).





##Load and Process Data 
First, we need to load and read in the data. We can see that the data consists of three variables, "steps", "date", and "interval".  

* Steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
* date: The date on which the measurement was taken in YYYY-MM-DD format  
* interval: Identifier for the 5-minute interval in which measurement was taken  

In addition, we will do a few pre-processing steps -- which will create dataframes that summarize the daily steps by date, and also by interval (time of day).


```r
fileurl="http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filename <- "activity.zip"
if (file.exists(filename)) {
  print("File present!")
  } else {download.file(fileurl, destfile="activity.zip")}
```

```
## [1] "File present!"
```

```r
#This is the main data
activity <- read.csv(unz("activity.zip","activity.csv"))
head(activity) 
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
#New dataframes by day and by interval
bydate <- summarize(group_by(activity, date), steps=sum(steps))
byint <- summarize(group_by(activity, interval), steps=mean(steps, na.rm=TRUE))
```

##What is mean total number of steps taken per day? 
Using a simple histogram, we get an idea of the average daily total steps. Additionally, the mean and median of the data are reported in the annotation in the plot below.  


```r
plot <- ggplot(data=bydate, aes(bydate$steps))
plot1 <- plot + geom_histogram(stat="bin",
                               binwidth=3000,
                               color="blue", 
                               fill="white")
plot1 <- plot1 + labs(title="Total number of steps taken per day",
                      x="Total steps per day",
                      y="Count of summed steps")
print(plot1)
```

![plot of chunk part2](figure/part2-1.png) 

```r
data.frame(Mean=mean(bydate$steps, na.rm=TRUE), Median=median(bydate$steps, na.rm=TRUE))
```

```
##       Mean Median
## 1 10766.19  10765
```

##What is the average daily activity pattern?
Earlier, a dataframe reporting average steps by each 5-min interval was created, so we can use that dataframe to make a line plot of activity at each 5-minute interval, averaged over all days. In the dataset, the 5-minute intervals are identified by an index from 0 to 2355.  


```r
plot2 <- ggplot(data=byint, aes(x=interval, y=steps))
plot2 <- plot2 + geom_line() +
  labs(title="Average Daily Activity\n in 5-minute intervals",
     x="Interval",
     y="Average Steps Taken")
plot2
```

![plot of chunk part3](figure/part3-1.png) 

```r
#calculate maximum avg. steps and its matching interval
maxint<-byint$interval[which.max(byint$steps)]
maxstep<-max(intstepsmean$meansteps)
data.frame(Max_Interval=maxint, Max_Steps=maxstep)
```

```
##   Max_Interval Max_Steps
## 1          835  206.1698
```

We also want to answer the question **"Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?"**  
The answer to this, as shown in the plot, is 206.1698113 steps (rounded to the nearest integer). The interval that corresponds to the max steps is 835, or 8:35-8:40 AM. 

##Imputing missing values
The original dataset had a missing values, signified by "NA", which we initially ignored. First we figure out how many missing values there are.


```r
complete <- complete.cases(activity)
missing <-length(complete[complete==FALSE])
present <- length(complete[complete==TRUE])
data.frame(Missing=missing, Complete=present)
```

```
##   Missing Complete
## 1    2304    15264
```

There are 2304 missing values, and 15264 complete cases in the original dataset. To fill in this data, we can use the average values from each 5-minute interval. We could have also used the daily average, but since the previous plot showed that there is a lot of variation by time of day, it seems "safer" to use the average at each 5-min interval.

To do this, we go back to the original dataset. We also use the interval means dataset (`byint`) created for the second plot, which provides us the mean interval values to substitute in at each missing datapoint.


```r
act2<-activity
for (i in 1:nrow(act2)) {
    if (is.na(act2$steps[i])) {
        act2$steps[i] <- byint[which(act2$interval[i] == byint$interval), ]$steps
    }
}
sum(is.na(act2)) #check that there's no more NAs
```

```
## [1] 0
```

```r
head(act2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

**Did the data change by our "impute" procedure?**
To look at the effect of [fill in]


```r
bydate2 <- summarize(group_by(act2, date), steps=sum(steps))
bydate_all <- data.frame(date=bydate$date, imputed=bydate2$steps, orig=bydate$steps)
plotdata <- melt(bydate_all, id="date")
plot3 <- ggplot(data=plotdata, aes(x=value, fill=variable))
plot3 <- plot3 + geom_histogram(stat="bin", binwidth=3000, position="dodge")
plot3
```

![plot of chunk part6](figure/part6-1.png) 

```r
means <- rbind(mean(bydate$steps, na.rm=TRUE),median(bydate$steps,na.rm=TRUE),mean(bydate2$steps),median(bydate2$steps))
labels <- c("Orig. Mean", "Orig. Median", "Imputed Mean", "Imputed Median")
cbind(labels, means)
```

```
##      labels                             
## [1,] "Orig. Mean"     "10766.1886792453"
## [2,] "Orig. Median"   "10765"           
## [3,] "Imputed Mean"   "10766.1886792453"
## [4,] "Imputed Median" "10766.1886792453"
```

The mean and median have not changed much by "imputing" the missing data. The main effect of the imputation is that the frequencies at each histogram bin has increased. 

##Are there differences in activity patterns between weekdays and weekends?


```r
#change date format in dataset
act2$date <- as.Date(strptime(act2$date, format="%Y-%m-%d"))
act2$dow <- weekdays(act2$date)
#Make the variable dow a factor with 2 levels
for (i in 1:nrow(act2)) {                                       
    if (act2[i,]$dow %in% c("Saturday","Sunday")) {             
        act2[i,]$dow<-"Weekend"                               
    }
    else{
       act2[i,]$dow<-"Weekday"                                
    }
}
act2_weekend <- group_by(act2, interval, dow)
weekend <- summarize(act2_weekend, mean(steps))
colnames(weekend)<- c("interval", "partofweek", "steps" )
plot <- ggplot(weekend, aes(x=interval, y=steps)) + geom_line() + facet_grid(.~partofweek, scales="free", space="free")
plot <- plot + labs(x="Interval", y="Mean Steps", title="Mean Steps by 5-min Interval\n Weekend vs. Weekdays") 
print(plot)
```

![plot of chunk part7](figure/part7-1.png) 
From this plot, we see that our anonymous walker walks more during weekends, but each weekday, there is a lot of walking right near the interval at 830 (which we sort of knew from before). The final graph gives us a clue as to the overall walking pattern: More walking on weekends, but on weekdays, our person does a lot of walking around ~8:30AM. This pattern fits with a person who maybe walks to work everyday, and then takes a lot of walks on weekend days. 



