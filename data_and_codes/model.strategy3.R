library(ggplot2)
library(reshape2)
library(stats)
library(caret)
library(reshape2)
library(signal)
library(tictoc)
library(earth)
library(mda)
library(Metrics)
library(lubridate)
suppressWarnings(suppressMessages(library(doParallel)))

data0 <- read.csv("train.csv")

data0$datetime   = as.POSIXct(data0$datetime, tz = "EST")
data0$year       = as.numeric(format(data0$datetime,"%Y"))
data0$month      = as.numeric(format(data0$datetime,"%m"))
data0$weekday    = factor(format(data0$datetime,"%A")) 
data0$hour       = as.numeric(as.POSIXlt(data0$datetime)$hour)

data0$holiday    = factor(data0$holiday)
data0$workingday = factor(data0$workingday)
data0$weather    = factor(data0$weather)
data0$atemp      = as.numeric(data0$atemp)
data0$humidity   = as.integer(data0$humidity)
data0$windspeed  = as.numeric(data0$windspeed)

data0$weekday    = factor(as.numeric(!(data0$weekday=="Saturday" | data0$weekday=="Sunday")))

features <- c("datetime",
              "year",
              "month",
              "holiday",
              "workingday",
              "weather",
              "atemp",
              "humidity",
              "windspeed",
              "weekday",
              "hour")

featureCols <- grep(paste0(features,collapse="|"),names(data0))
finalData <- cbind(data.frame(count=data0$count),data0[,featureCols])

months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

## Modeling
# For validation, we will use data from the 16th to 19th of each month
set.seed(1234)

minTrainDay <- 16
inTrain  <- seq(1,nrow(finalData))[day(ymd_hms(data0$datetime)) < minTrainDay]

training0 <- finalData[inTrain,]
testing0  <- finalData[-inTrain,]

for (cyear in 2011:2012) {
  for (cmth in 1:12) {
    
    onset <- ymd_hms(as.POSIXct(paste0(c(cyear,"-",cmth,"-20 00:00:01"),collapse=""),tz="EST"))
    training <- training0[ymd_hms(training0$datetime) <= onset,]
    
    removeCols <- grep("datetime",names(training))
    training <- training[,-removeCols]
    
    # Train
    cl <- makeCluster(3)
    registerDoParallel(cl)

    tic();fit.rf <- train(count ~ .,method="rf",data=training);toc()
    tic();fit.earth <- train(count ~ .,method="earth",data=training);toc()

    stopCluster(cl)

    # Save model to disk
    prefix <- "saved_models_accumulate_wmthyr/fit."
    suffix <- c(cyear,"_",months[cmth],".RData")

    filename <- paste0(c(prefix,"rf",".",suffix),collapse="")
    save(fit.rf,file=filename)

    filename <- paste0(c(prefix,"earth",".",suffix),collapse="")
    save(fit.earth,file=filename)
  }
}







