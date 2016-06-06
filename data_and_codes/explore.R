library(ggplot2)
library(reshape2)

data0 <- read.csv("train.csv")

data0$datetime = as.POSIXct(data0$datetime, tz = "EST")

data0$year      = format(data0$datetime,"%Y") 
data0$month     = format(data0$datetime,"%b") 
data0$weekday   = format(data0$datetime,"%A") 
data0$monthday  = format(data0$datetime,"%d")
data0$hour      = as.POSIXlt(data0$datetime)$hour
data0$julianday = as.POSIXlt(data0$datetime)$yday

data0$month = factor(data0$month,
                       levels = c("Jan","Feb","Mar","Apr","May", "Jun",
                                  "Jul","Aug","Sep","Oct","Nov","Dec"))

data0$weekday = factor(data0$weekday,
                       levels = c("Monday", "Tuesday","Wednesday", 
                                  "Thursday", "Friday", "Saturday",
                                  "Sunday"))

# Aggregate by workingday
xx <- aggregate(data0$count, 
                list(hour=data0$hour,
                     workingday=factor(data0$workingday)), 
                FUN = "mean")

gg <- ggplot(xx,aes(x=hour,y=x,color=workingday))
gg <- gg + geom_line()
plot(gg)

# Aggregate by holiday
xx <- aggregate(data0$count, 
                list(hour=data0$hour,
                     holiday=factor(data0$holiday)), 
                FUN = "mean")

gg <- ggplot(xx,aes(x=hour,y=x,color=holiday))
gg <- gg + geom_line()
plot(gg)

# Aggregate by weekday
xx <- aggregate(data0$count, 
               list(hour=data0$hour,weekday=data0$weekday), 
               FUN = "mean")

gg <- ggplot(xx,aes(x=hour,y=x,color=weekday))
gg <- gg + geom_line()
plot(gg)

## Aggregate by monthday
xx <- aggregate(data0$count, 
                list(monthday=data0$monthday,month=data0$month), 
                FUN = "mean")

gg <- ggplot(xx,aes(x=monthday,y=x))
gg <- gg + geom_boxplot()
plot(gg)

## Aggregate by month
xx <- aggregate(data0$count, 
               list(monthday=data0$monthday,month=data0$month), 
               FUN = "mean")

gg <- ggplot(xx,aes(x=as.numeric(monthday),y=x,color=month))
gg <- gg + geom_line()
plot(gg)

## Aggregate by month2
xx <- aggregate(data0$count, 
                list(monthday=data0$monthday,month=data0$month,
                     year=data0$year), 
                FUN = "mean")

gg <- ggplot(xx,aes(x=month,y=x))
gg <- gg + geom_violin()
gg <- gg + facet_grid(year ~ .)
plot(gg)

## Count vs feels-like temperature
gg <- ggplot(data0,aes(x=atemp,y=count,color=humidity))
gg <- gg + geom_point(alpha=0.5)
gg <- gg + facet_grid(year ~ .)
plot(gg)

## Count vs humidity
gg <- ggplot(data0,aes(x=humidity,y=count,color=windspeed))
gg <- gg + geom_point(alpha=0.5)
gg <- gg + facet_grid(year ~ .)
plot(gg)


## Count vs season
gg <- ggplot(data0,aes(x=factor(season),y=count))
gg <- gg + geom_violin()
gg <- gg + facet_grid(year ~ .)
plot(gg)

## Count vs windspeed
gg <- ggplot(data0,aes(x=windspeed,y=count,color=factor(season)))
gg <- gg + geom_point(alpha=0.5)
gg <- gg + facet_grid(year ~ .)
plot(gg)

## Count vs weather
gg <- ggplot(data0,aes(x=factor(weather),y=count))
gg <- gg + geom_violin()
gg <- gg + facet_grid(year ~ .)
plot(gg)

# check for missing values
a <- NULL
print(names(data0))
for (i in seq(1,ncol(data0))) {
  vec <- data0[,i]
  a <- c(a,(sum(is.na(vec))))
}

for (i in 1:ncol(data0)) {
  print(c(class(data0[,i]),names(data0)[i]))
}
# [1] "POSIXct"  "POSIXt"   "datetime"
# [1] "integer" "season" 
# [1] "integer" "holiday"
# [1] "integer"    "workingday"
# [1] "integer" "weather"
# [1] "numeric" "temp"   
# [1] "numeric" "atemp"  
# [1] "integer"  "humidity"
# [1] "numeric"   "windspeed"
# [1] "integer" "casual" 
# [1] "integer"    "registered"
# [1] "integer" "count"  
# [1] "character" "year"     
# [1] "factor" "month" 
# [1] "factor"  "weekday"
# [1] "character" "monthday" 
# [1] "integer" "hour" 

data0$season     = factor(data0$season)
data0$holiday    = factor(data0$holiday)
data0$workingday = factor(data0$workingday)
data0$weather    = factor(data0$weather)
data0$temp       = as.numeric(data0$temp)
data0$atemp      = as.numeric(data0$atemp)
data0$humidity   = as.integer(data0$humidity)
data0$windspeed  = as.numeric(data0$windspeed)
data0$year       = as.integer(data0$year)
data0$month      = factor(data0$month)
data0$weekday    = factor(data0$weekday)
data0$hour       = factor(data0$hour) 
data0$julianday  = as.integer(data0$julianday) 

for (i in 1:ncol(data0)) {
  print(c(class(data0[,i]),names(data0)[i]))
}

features <- c("season",
              "holiday",
              "workingday",
              "weather",
              "temp",
              "atemp",
              "humidity",
              "windspeed",
              "year",
              "month",
              "weekday",
              "hour",
              "julianday")

featureCols <- grep(paste0(features,collapse="|"),names(data0))
finalData <- cbind(data.frame(count=data0$count),data0[,featureCols])

# dates
# print(head(as.Date(data0$datetime)))
# print(head(data0$month))
# print(head(data0$hour,48))



# ## Modeling
# set.seed(1234)
# minTrainDay <- 16
# #inTrain  <- createDataPartition(y=data0$Sand, p=0.75, list=FALSE)
# inTrain  <- seq(1,nrow(finalData))[finalData$monthday < minTrainDay]
# 
# training <- finalData[inTrain,]
# testing  <- finalData[-inTrain,]
# 
# months <- unique(as.character(training$month))
# for (yr in c(2011,2012)){
#   for (i in 1:12) {
#     mthnums <- 1:i
#     mthvals <- months[mthnums]
#     mthcat  <- paste0(mthvals,collapse="|")
#     mthrows <- grep(mthcat,training$month)
#     
#     training = training[mthrows,]
#     training = training[training$year == yr,]
#     
#     if (yr == 2011 & i == 1) {
#       
#       cl <- makeCluster(3)
#       registerDoParallel(cl)
#       
#       tic(); fitSand.lm.50          <- train(Sand ~ ., method="lm",          data=trainingSand); toc()
#       tic(); fitSand.bagEarth.50    <- train(Sand ~ ., method="bagEarth",    data=trainingSand); toc()
#       tic(); fitSand.bagEarthGCV.50 <- train(Sand ~ ., method="bagEarthGCV", data=trainingSand); toc()
#       tic(); fitSand.earth.50       <- train(Sand ~ ., method="earth",       data=trainingSand); toc()
#       tic(); fitSand.gcvEarth.50    <- train(Sand ~ ., method="gcvEarth",    data=trainingSand); toc()
#       tic(); fitSand.rf.50          <- train(Sand ~ ., method="rf",          data=trainingSand); toc()
#       
#       stopCluster(cl)
#     }
#     
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
