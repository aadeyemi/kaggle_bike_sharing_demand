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
data0$weekday    = factor(format(data0$datetime,"%A")) 
data0$hour       = as.POSIXlt(data0$datetime)$hour

data0$holiday    = factor(data0$holiday)
data0$workingday = factor(data0$workingday)
data0$weather    = factor(data0$weather)
data0$atemp      = as.numeric(data0$atemp)
data0$humidity   = as.integer(data0$humidity)
data0$windspeed  = as.numeric(data0$windspeed)

data0$weekday    = factor(as.numeric(!(data0$weekday=="Saturday" | data0$weekday=="Sunday")))
data0$hour       = as.numeric(data0$hour) 

features <- c("datetime",
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

insample.rf.error.vec <- outsample.rf.error.vec <- insample.earth.error.vec <- outsample.earth.error.vec <- NULL
rf.id <- earth.id <- NULL

## Modeling
# For validation, we will use data from the 16th to 19th of each month
set.seed(1234)

minTrainDay <- 16
inTrain  <- seq(1,nrow(finalData))[day(ymd_hms(data0$datetime)) < minTrainDay]

training0 <- finalData[inTrain,]
testing0  <- finalData[-inTrain,]

for (cyear in 2011:2012) {
    for (cmth in 1:12) {
        onset1 <- ymd_hms(as.POSIXct(paste0(c(cyear,"-",cmth,  "-01 00:00:00"),collapse=""),tz="EST")-1)
        onset2 <- ymd_hms(as.POSIXct(paste0(c(cyear,"-",cmth,  "-28 00:00:00"),collapse=""),tz="EST")-1)
        
        training <- training0[ymd_hms(training0$datetime) >= onset1 & ymd_hms(training0$datetime) <= onset2,]
        testing  <- testing0 [ymd_hms(testing0$datetime)  >= onset1 & ymd_hms(testing0$datetime)  <= onset2,]
        
        removeCols <- grep("datetime",names(training))
        training <- training[,-removeCols]
        
        removeCols <- grep("datetime",names(testing))
        testing <- testing[,-removeCols]
        
        # Load model from disk
        prefix <- "saved_models_permonth/fit."
        suffix <- c(cyear,"_",months[cmth],".RData")
        
        filename <- paste0(c(prefix,"rf",".",suffix),collapse="")
        load(file=filename) #fit.rf
        
        filename <- paste0(c(prefix,"earth",".",suffix),collapse="")
        load(file=filename) #fit.earth
        
        # predict rf training and testing
        pred.rf.train <- predict(fit.rf,training)
        pred.rf.test  <- predict(fit.rf,testing)
        
        insample.rf.error  = rmse(training$count,pred.rf.train)
        outsample.rf.error = rmse(testing$count,pred.rf.test)
        
        # predict earth training and testing
        pred.earth.train <- predict(fit.earth,training)
        pred.earth.test <- predict(fit.earth,testing)
        
        insample.earth.error  <- rmse(training$count,pred.earth.train)
        outsample.earth.error <- rmse(testing$count,pred.earth.test)
        
        # accumulate results
        insample.rf.error.vec <- c(insample.rf.error.vec,insample.rf.error)
        outsample.rf.error.vec <- c(outsample.rf.error.vec,outsample.rf.error)
        rf.id <- c(rf.id,"rf")
        insample.earth.error.vec <- c(insample.earth.error.vec,insample.earth.error)
        outsample.earth.error.vec <- c(outsample.earth.error.vec,outsample.earth.error)
        earth.id <- c(earth.id,"earth")
    }
}

errors3 <- data.frame(in.sample.error = c(insample.rf.error.vec,insample.earth.error.vec),
                     out.sample.error = c(outsample.rf.error.vec,outsample.earth.error.vec),
                     type = c(rf.id,earth.id))

write.csv(errors3, file = "errors_permonth.csv")

gg <- ggplot(errors,aes(x=in.sample.error,y=out.sample.error,color=type))
gg <- gg + geom_point(alpha=1)
#gg <- gg + xlim(c(1,150)) + ylim(c(1,150))
gg <- gg + geom_abline(intercept = 0, slope = 1, linetype = "dashed")
plot(gg)



