Introduction
------------

This document details my exploratory data analysis prior to building a
prediction model for the [Kaggle Bike Sharing Demand
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

My goal is to find relationships in the provided data in order to help
select the best input to my prediction models.

Data
----

My exploratory data analysis uses the training data set available at
<https://www.kaggle.com/c/bike-sharing-demand/data>. This data set comes
in csv format where each row contains the following data fields:

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

First I need to load the data set to my work session and check for
completeness.

    data0 <- read.csv("data_and_codes/train.csv")
    a <- NULL
    for (i in seq(1,ncol(data0))) a <- sum(a,(sum(is.na(data0[,i]))))

There are 0 missing data entries.

Although we are given the datetime field, it would be useful to create
independent fields containing the year, month, day, and hour of each
record.

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

    data0$weekday2 = factor(
      as.numeric(!(data0$weekday=="Saturday" | data0$weekday=="Sunday")))
    levels(data0$weekday2)[levels(data0$weekday2)=="1"] <- "Weekday"
    levels(data0$weekday2)[levels(data0$weekday2)=="0"] <- "Weekend"

Exploring the data set
----------------------

By asking some key questions, I hope to be able to find strong
correlations or relationships between the provided variables and the
outcome variables.

### 1. what is the statistical distribution of bike rental counts?

My first objective is to know the statistical distribution of bike
counts. What is the overall distribution and how does it vary by year.
![](exploratory_data_analysis_files/figure-markdown_strict/plot%20density%20plot%20for%20bike%20rental%20count-1.png)<!-- -->

<table>
<thead>
<tr class="header">
<th align="left">year</th>
<th align="right">mean</th>
<th align="right">sd</th>
<th align="right">median</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">2011</td>
<td align="right">144</td>
<td align="right">133</td>
<td align="right">111</td>
</tr>
<tr class="even">
<td align="left">2012</td>
<td align="right">239</td>
<td align="right">208</td>
<td align="right">199</td>
</tr>
<tr class="odd">
<td align="left">overall</td>
<td align="right">192</td>
<td align="right">181</td>
<td align="right">145</td>
</tr>
</tbody>
</table>

Table 1. Statistical properties of bike rental count from 2011 to 2012.

From Figure 1, we can see that the distribution is skewed to the right
with a long tail. Table 1 shows an increase in number of rentals from
2011 to 2012. This is a long-term trend considering the fact that we are
interested in hourly rental counts.

### 2. Does the number of rented bikes vary by hour?

Aggregate all data by hour and find the average count for each hour.

    data <- aggregate(data0$count,list(hour=data0$hour),FUN = "mean")

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20hourly%20variation-1.png)<!-- -->

Figure 2 shows that there is hourly variation in the number of bikes
rented. This is a short-term trend.

### 3. Does the hourly variation observed depend on what day of the week it is?

Aggregate all data by hour as well as by day, then find the average
count for each hour.

    data <- aggregate(data0$count,list(hour=data0$hour,weekday=data0$weekday),FUN="mean")

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20daily%20variation-1.png)<!-- -->

Figure 3 shows hourly variation in the number of bikes rented that is
dependent on day of week. This is revealing but not surprising since we
would expect weekday patterns to be different from weekend patterns.

We can aggregate this plot further by merging and averaging weekday
hourly rentals and weekend hourly rentals. We can even draw spline
curves through the data points "to see beyond the kinks".

    data <- aggregate(data0$count,list(hour=data0$hour,Day=data0$weekday2),FUN="mean")

    weekday0 <- data[data$Day=="Weekend",]
    weekday0 <- spline(weekday0$x, n=length(weekday0$x)*10)
    weekday0 <- as.data.frame(weekday0)
    names(weekday0) <- c("hour","x")

    weekday1 <- data[data$Day=="Weekday",]
    weekday1 <- spline(weekday1$x, n=length(weekday1$x)*10)
    weekday1 <- as.data.frame(weekday1)
    names(weekday1) <- c("hour","x")

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20smooth%20daily-1.png)<!-- -->

During weekdays there are more rentals early in the morning and later
afternoons while during the weekends, there is a peak at around 1pm.

### 4. Not all holidays are weekends and not all weekends are holidays. Does the hourly variation observed depend on whether it is a holiday or not? Does the resulting pattern mimic that of the weekday/weekend?

Aggregate all data by hour as well as by holiday indicator, then find
the average count for each hour.

    data <- aggregate(data0$count,list(hour=data0$hour,holiday=factor(data0$holiday)),FUN="mean")

    levels(data$holiday)[levels(data$holiday)=="1"] <- "Holiday"
    levels(data$holiday)[levels(data$holiday)=="0"] <- "Non-Holiday"

    holiday0 <- data[data$holiday=="Non-Holiday",]
    holiday0 <- spline(holiday0$x, n=length(holiday0$x)*10)
    holiday0 <- as.data.frame(holiday0)
    names(holiday0) <- c("hour","x")

    holiday1 <- data[data$holiday=="Holiday",]
    holiday1 <- spline(holiday1$x, n=length(holiday1$x)*10)
    holiday1 <- as.data.frame(holiday1)
    names(holiday1) <- c("hour","x")

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20smooth%20holiday-1.png)<!-- -->

Figure 5 shows that the hourly variation in the number of bikes rented
that is dependent whether the day is a holiday or not. This relationship
is similar to that of weekday/weekend but not exactly the same because
holidays can either fall on weekdays or weekends. Also, generally
speaking, holiday demands are somewhat different from weekend demands in
most situations.

### 5. We were provided information on whether the day is a working day or not. Is the corresponding average hourly bike rental pattern different from that seen in the weekend/weekday or holiday/non-holiday patterns?

Aggregate all data by hour as well as by working-day indicator, then
find the average count for each hour.

    data <- aggregate(data0$count,list(hour=data0$hour,workingday=factor(data0$workingday)),FUN="mean")

    levels(data$workingday)[levels(data$workingday)=="1"] <- "Working"
    levels(data$workingday)[levels(data$workingday)=="0"] <- "Non-Working"

    working0 <- data[data$workingday=="Non-Working",]
    working0 <- spline(working0$x, n=length(working0$x)*10)
    working0 <- as.data.frame(working0)
    names(working0) <- c("hour","x")

    working1 <- data[data$workingday=="Working",]
    working1 <- spline(working1$x, n=length(working1$x)*10)
    working1 <- as.data.frame(working1)
    names(working1) <- c("hour","x")

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20smooth%20working%20day-1.png)<!-- -->

Figure 6 shows a pattern that is very similar to that seen in
weekday/weekend.

### 6. We will have to use data from the 1st to 19th of each month to train our models. Are rental distribution dependent on what day of the month? Can we find a pattern that indicates that it would be impossible to train a model for dates not represented in our training data set?

Aggregate data by month day, then plot distribution with boxplots.

    data <- aggregate(data0$count,list(monthday=data0$monthday,month=data0$month),FUN = "mean")

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20variation%20by%20month%20day-1.png)<!-- -->
The boxplots shown in Figure 7 suggest that we do not necessarily have
to have all month days in our training data sets.

### 7. What is the monthly variation in bike rental?

I will expect the answer to this question to reflect seasons of the
year. People are more likely to ride bikes when temperatures are not
harsh.

    data <- aggregate(data0$count,list(monthday=data0$monthday,month=data0$month,year=data0$year),FUN = "mean")

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20monthly%20variation-1.png)<!-- -->

As expected, we see variation in bike rental by month. Low bike rental
count in the winter months, and higher counts in the spring/summer/fall
months. The numbers also change with year reflecting an increase in
popularity of the bike rental program from 2011 to 2012. Just like the
the yearly increase trend, this monthly variation is also a long-term
trend.

### 8. Does temperature have an effect on bike rental count?

Plot bike rental count as a function of the "feels-like" and actual
temperature.
![](exploratory_data_analysis_files/figure-markdown_strict/plot%20feels%20like%20temperature%20variation-1.png)<!-- -->

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20temperature%20variation-1.png)<!-- -->

We see that there is an optimal range of temperatures for which we get
the most rentals. Outside of this range, cooler or hotter, numbers are
lower.

### 9. Does humidity have an effect on bike rental count?

Plot bike rental count as a function of humidity.
![](exploratory_data_analysis_files/figure-markdown_strict/plot%20humidity%20variation%201-1.png)<!-- -->

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20humidity%20variation%202-1.png)<!-- -->

A very weak humidity pattern is seen in the plots. Humidity does not
play as much of a significant role as the previously discussed
variables.

### 10. Does windspeed have an effect on bike rental count?

Plot bike rental count as a function of wind-speed.

![](exploratory_data_analysis_files/figure-markdown_strict/plot%20windspeed%20variation%201-1.png)<!-- -->

Figure 13 shows a pattern in the hourly variation in bike rental with
wind-speed. Bike rental count tends to be higher with lower wind-speed.
This reflects difficulties experienced by riders when wind-speed is
high.

### 11. Does season have an effect on bike rental count?

Plot bike rental count as a function of season.
![](exploratory_data_analysis_files/figure-markdown_strict/plot%20season%20variation%201-1.png)<!-- -->

As expected, bike rentals are affected by season. This is reflective of
some of the earlier discussed variables that have direct impact on the
bike rider. the winter season has the lowest values of all the seasons.
We saw this in the plot that shows monthly variations.

### 12. Do the provided weather categories have an effect on bike rental count?

Plot bike rental count as a function of weather.
![](exploratory_data_analysis_files/figure-markdown_strict/plot%20weather%20variation-1.png)<!-- -->

As expected, zero counts for weather category 4 defined as *Heavy Rain +
Ice Pallets + Thunderstorm + Mist, Snow + Fog*. That would be a tough
situation to ride a bike in. Weather categories 1 and 2 seem to be the
most favorable for bike rental. Fewer rentals in weather category 3.

Conclusions
-----------

I have performed exploratory data analysis on the [Kaggle Bike Sharing
Demand Data Set](!https://www.kaggle.com/c/bike-sharing-demand) and have
found plausible relationships between observed predictor variables
(date/time, day-type, weather, season, temperature and wind-speed) and
the desired outcome variable (bike rental hourly count). I found
long-term trends and short-term trends. Long-trends include yearly
increase in bike rental count arising from increase in popularity of the
program from 2011 to 2012, and monthly variations in bike rental count
arising from seasonal weather changes with month.

When modeling this data set, the impacts of the observed long-term and
short-term variations in rental bike count should be explored.
