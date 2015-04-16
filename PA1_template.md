# Reproducible Research: Peer Assessment 1
`r Sys.Date()`  
## Documenting the current environment

I know this was not part of the template for this assignment, but Prof Peng 
mentions in the week 3 lectures that part of the Reproducible Research workflow 
is to document the environment that the analysis is run in (just in case there 
are issues that relate to a specific version).  

I have therefore put in this 
section to remind myself later how to do this...


```r
# first load the required libraries
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# then generate the snapshot of the environment used
sessionInfo()
```

```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] dplyr_0.4.1
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1  DBI_0.3.1       digest_0.6.8    evaluate_0.5.5 
##  [5] formatR_1.1     htmltools_0.2.6 knitr_1.9       magrittr_1.5   
##  [9] parallel_3.1.3  Rcpp_0.11.5     rmarkdown_0.5.1 stringr_0.6.2  
## [13] tools_3.1.3     yaml_2.1.13
```

## Loading and preprocessing the data

Need to unzip the supplied file, read it into memory, and then clean up the
unzipped files.


```r
unzip("activity.zip", exdir = "unzipped")
activity <- read.csv("unzipped/activity.csv")
file.remove(c(list.files(path = "unzipped", recursive = TRUE, full.names = TRUE),
                      "unzipped"))
```

```
## [1] TRUE TRUE
```

## What is mean total number of steps taken per day?

There are a lot of NA's in the step count:

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
I will therefore remove the NA's before calculating the daily total step count:

```r
daily_totals <- activity %>% group_by(date) %>% na.omit %>% summarise(total = sum(steps))
daily_totals
```

```
## Source: local data frame [53 x 2]
## 
##          date total
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```
This allows me to plot the required histogram:

```r
hist(daily_totals$total, breaks=25L, col="dark blue", 
     main="Frequency distribution of the total number of steps per day")
```

![](PA1_template_files/figure-html/daily_totals_hist-1.png) 

The mean number of steps per day is 10,766.19 steps.  

The median number of steps per day is 10,765 steps.  

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
