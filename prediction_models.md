Introduction
------------

This document details my exploration of plausible prediction models for
the [Kaggle Bike Sharing Demand
Challenge](!https://www.kaggle.com/c/bike-sharing-demand). Details about
the challenge can be found at
<https://www.kaggle.com/c/bike-sharing-demand>. Participants were asked
to combine several quantities that could potentially affect the count of
bikes rented in the Capital Bikeshare program in Washington, D.C., in
order to forecast bike rental demand.

The provided training data set consists of measured and observed data
from the first 19 days of each month in 2011 and 2012, with data from
the remaining days of the month comprising the testing data set. For
each test day, only data from prior days and months will be used to
generate its prediction model.

My goal is to use what I learned about relationships between predictor
variables and dependence of the outcome variable on predictor variables
during exploratory data analysis to create plausible prediction models
for the data set.

Data
----

The [Kaggle Bike Sharing Demand Challenge data
set](!https://www.kaggle.com/c/bike-sharing-demand/data) comes in csv
format where each row contains the following fields:

-   **datetime** - hourly date + timestamp
-   **season** - 1 = spring, 2 = summer, 3 = fall, 4 = winter
-   **holiday** - whether the day is considered a holiday
-   **workingday** - whether the day is neither a weekend nor holiday
-   **weather** - 1: Clear, Few clouds, Partly cloudy, Partly cloudy; 2:
    Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist; 3:
    Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light
    Rain + Scattered clouds; 4: Heavy Rain + Ice Pallets +
    Thunderstorm + Mist, Snow + Fog
-   **temp** - temperature in Celsius
-   **atemp** - "feels like" temperature in Celsius
-   **humidity** - relative humidity
-   **windspeed** - wind speed
-   **casual** - number of non-registered user rentals initiated
-   **registered** - number of registered user rentals initiated
-   **count** - number of total rentals

The count field is the outcome variable given the other fields
(predictor variables). In the following sections, I will explore
relationships between the outcome variable (count), and the predictor
variables.

From the datetime field, we can extract independent year, month, and
hour fields. These are variables which had bike rental count trends.

    data0 <- read.csv("data_and_codes/train.csv")

    data0$datetime   = as.POSIXct(data0$datetime, tz = "EST")
    data0$year       = factor(format(data0$datetime,"%Y")) 
    data0$month      = factor(format(data0$datetime,"%m")) 
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

Feature Selection
-----------------

From exploratory data analysis, I learned that there are long-term
trends and short-term trends in the data set. Depending on what input
data we wish to use in prediction, we may choose to exclude certain
variables. For this Kaggle Challenge, participants were required to use
only prior data to the data and time of interest in building a
prediction model. All training data set were drawn from the 1st to 19th
of each month and testing data sets from the 20th to the end of the
month. For validation, I have chosen to use data from the 1st to the
15th of each month for model training and 16th to 19th for validation.

I devised three strategies in model building:

-   **Strategy 1: Short-term prediction** - Strategy 1 uses only data
    from the 1st to 15th of each month to build prediction models for
    that month. This will capture only the most recent short term trends
    to the date and hour of interest.
-   **Strategy 2: Long-term prediction 1** - Strategy 2 uses all
    available prior data to build prediction models for the date and
    hour of interest, but does not explicitly use month and year
    as features. This should capture the most recent short-term as well
    as the prior long term trends in the data set.
-   **Strategy 3: Long-term prediction 2** - Strategy 3 uses all
    available prior data to build prediction models for the date and
    hour of interest, and explicitly uses month and year as features.
    This should capture the most recent short-term as well as the prior
    long term trends in the data set.

My final feature set is as follows: holiday (factor), workingday
(factor), weekday (factor), weather (factor), humidity (integer), atemp
(numeric), windspeed (numeric), hour (numeric), month (numeric), year
(numeric).

While month and year are excluded in strategies 1 and 2, they are
included in strategy 3.

Model Building
--------------

I built prediction models using the earth and random forest methods and
the R codes for the three strategies described earlier are shown below:

### Strategy 1 model building code

    features <- c("datetime","holiday","workingday","weather","atemp","humidity","windspeed","weekday","hour")
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
            
            onset1 <- ymd_hms(as.POSIXct(paste0(c(cyear,"-",cmth,"-01 00:00:00"),collapse=""),tz="EST")-1)
            onset2 <- ymd_hms(as.POSIXct(paste0(c(cyear,"-",cmth,"-20 00:00:01"),collapse=""),tz="EST"))
            
            training <- training0[ymd_hms(training0$datetime) >= onset1 & ymd_hms(training0$datetime) <= onset2,]
            
            removeCols <- grep("datetime",names(training))
            training <- training[,-removeCols]
            
            # Train
            tic();fit.rf <- train(count ~ .,method="rf",data=training);toc()
            tic();fit.earth <- train(count ~ .,method="earth",data=training);toc()
            
            # Save model to disk
            prefix <- "data_and_codes/saved_models_permonth/fit."
            suffix <- c(cyear,"_",months[cmth],".RData")
            
            filename <- paste0(c(prefix,"rf",".",suffix),collapse="")
            save(fit.rf,file=filename)
            
            filename <- paste0(c(prefix,"earth",".",suffix),collapse="")
            save(fit.earth,file=filename)
        }
    }

### Strategy 2 model building code

    features <- c("datetime","holiday","workingday","weather","atemp","humidity","windspeed","weekday","hour")

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
        tic();fit.rf <- train(count ~ .,method="rf",data=training);toc()
        tic();fit.earth <- train(count ~ .,method="earth",data=training);toc()
        
        # Save model to disk
        prefix <- "data_and_codes/saved_models_accumulate/fit."
        suffix <- c(cyear,"_",months[cmth],".RData")
        
        filename <- paste0(c(prefix,"rf",".",suffix),collapse="")
        save(fit.rf,file=filename)
        
        filename <- paste0(c(prefix,"earth",".",suffix),collapse="")
        save(fit.earth,file=filename)
      }
    }

### Strategy 3 model building code

    features <- c("datetime","year","month","holiday","workingday","weather","atemp","humidity","windspeed","weekday","hour")

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
        tic();fit.rf <- train(count ~ .,method="rf",data=training);toc()
        tic();fit.earth <- train(count ~ .,method="earth",data=training);toc()

        # Save model to disk
        prefix <- "data_and_codes/saved_models_accumulate_wmthyr/fit."
        suffix <- c(cyear,"_",months[cmth],".RData")

        filename <- paste0(c(prefix,"rf",".",suffix),collapse="")
        save(fit.rf,file=filename)

        filename <- paste0(c(prefix,"earth",".",suffix),collapse="")
        save(fit.earth,file=filename)
      }
    }

Results
-------

Once models have been built, we can assess results by examining the
prediction errors of our models. In this case, I choose to use the root
mean square error (rmse). Scatter plots of out-of-sample error vs
in-sample error can help examine the performance of our models. As much
as I want our models to have very low in-sample errors, I would also
like my out-of-sample error to be close to my in-sample error since one
of my assumptions is that the outcome variables all come from the same
distribution so any errors in predicting one subset of it should be
close to errors in predicting another subset of it. I want the in-sample
error to my expected out-of-sample error.

### Strategy 1

    ## Loading required package: randomForest

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

![](prediction_models_files/figure-markdown_strict/plot%20predict%20strategy%201-1.png)<!-- -->

Figure 1 shows that for all 24 months predicted, random forest
out-performs earth method, however, in-sample errors better reflect
out-of-sample errors using the earth method.

### Strategy 2

![](prediction_models_files/figure-markdown_strict/plot%20predict%20strategy%202-1.png)<!-- -->

Figure 2 also shows that for all 24 months predicted, random forest
out-performs earth method especially in regards to in-sample errors. In
the 2012 months both methods produce larger in-sample and out-of-sample
errors when compared to Strategy 1. This is attributed to the
non-inclusion of data supporting the long term trend present in the
data.

### Strategy 3

![](prediction_models_files/figure-markdown_strict/plot%20predict%20strategy%203-1.png)<!-- -->

We can see in Figure 3 that using year and month as features in
long-term prediction improves both in-sample and out-of-sample errors
overall.

### Strategy comparison

![](prediction_models_files/figure-markdown_strict/plot%20predict%20all%20strategies-1.png)<!-- -->

Of the three strategies discussed, Strategy 2 seems to perform worst.
There are pros and cons to using Strategies 1 and 3. With Strategy 1,
in-sample error better reflect out-of-sample error but in-sample errors
are slightly larger. However, with Strategy 3, in-sample errors are
slightly lower but are not as good as the in-sample errors from Strategy
1 in reflecting the out-of-sample errors.

### Sample prediction plots

Plots of examples of training data and validation data predictions are
shown below.

![](prediction_models_files/figure-markdown_strict/plot%20rf%20prediction%201-1.png)<!-- -->

![](prediction_models_files/figure-markdown_strict/plot%20rf%20prediction%202-1.png)<!-- -->

Conclusions
-----------

Using knowledge gained by exploratory data analysis, I devised three
strategies for building prediction models for bike rental count from
provided historical data. Strategies differed based on how much of the
historical data are used in prediction. If only recent data (&lt; 3
weeks) are used, it is unnecessary to include features for month and
year. However if all historical data are to be used, it is necessary to
include month and year so as to capture and account for the long-term
trends in the bike rental. If these are not included, larger prediction
rms errors will be obtained.

Recommendations
---------------

While I have trained my hourly bike rental count predictors using
similar algorithm tuning parameters for each month, better results can
be obtained if models are trained using tuning parameters best suited
for the historical data available for each month independently. In other
words, when training your model for the month in question, tuning
parameters that produce the best in-sample and out-of-sample errors
should be used.
