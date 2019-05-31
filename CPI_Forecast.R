# Name: Iarlaith McLaughlin
# Student Number: L00144319
# Date: 29/05/2019
# Course: MSc. in Big Data Analysis
# Assignment: CA4 - Predictive Modelling

# Source for Consumer Price Index by month: https://www.cso.ie/en/statistics/prices/consumerpriceindex/

# Loading libraries
library('ggplot2')
library('forecast')
library('tseries')
library('zoo')

# The below will read the Consumer Price Index CSV file into a dataframe called "monthly_CPI". This data set
# has used Jan of 2005 as it's start point and all other values are marked as a percentage of this value e.g 
# Jan 2005 has a value of 100 (100%) and Jan 2019 was 113.8 (13.8% increase in comparison to Jan 2005). This data set
# has records from Nov 1975 to April 2019.

monthly_CPI <- read.csv('CPI.csv', header = TRUE, stringsAsFactors = FALSE)
head(monthly_CPI)
nrow(monthly_CPI)
summary(monthly_CPI)
str(monthly_CPI)

# Using the yearmon and as.Date functions to change the format of the date in the data frame.
monthly_CPI$Date <- as.yearmon(monthly_CPI$Date)
monthly_CPI$Date <- as.Date(monthly_CPI$Date)
str(monthly_CPI)
head(monthly_CPI)

# Plot of the CPI from 1975 to 2019.
p1 = ggplot(monthly_CPI, aes(x=Date, y=value, color=Type)) +
  geom_line() +
  labs(title = "All Consumer Price Indexes Nov 1975 - Apr 2019 (Base Jan 2005 = 100)", 
       x = "Year", y = "Percentage Change") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")
print(p1)

# Summary
summary(monthly_CPI)

# Transform the data frame to time series object
ts_monthly_CPI <- ts(monthly_CPI$value, frequency = 12, start = c(1975,11)) # freq 12 => Monthly data. 
plot(ts_monthly_CPI, main = "Monthly CPI from Nov-1975 to Apr-2019")                     
summary(ts_monthly_CPI)

# To check time series information
start(ts_monthly_CPI)
end(ts_monthly_CPI)
frequency(ts_monthly_CPI)

# Check stationarity: If a time series has a trend or seasonality component, 
# it must be made stationary before ARIMA can be used to forecast.

# Using the Augmented Dickey-Fuller test statistic to tell if the data is stationary. 
adf.test(ts_monthly_CPI)
# Data is not stationary as p-value is above 0.05.
# The null-hypothesis for an ADF test is that the data are non-stationary. 
# So large p-values are indicative of non-stationarity, and small p-values suggest stationarity.

# Can also run kpss test. Null hypothesis here is that data IS stationary.
kpss.test(ts_monthly_CPI)
# Result also suggests data is non-stationary at present.

# Can also check data using Acf and Pacf plots.
Acf_result <- Acf(ts_monthly_CPI)
# The result here in that the points are gradually dropping towards zero so at the moment it's non-stationary.

# After plotting the ACF plot we move to Partial Autocorrelation Function plots (PACF). A partial autocorrelation 
# is a summary of the relationship between an observation in a time series with observations at prior time steps 
# with the relationships of intervening observations removed.
Pacf_result <- Pacf(ts_monthly_CPI)
# Note that the PACF plot has a significant spike only at lag 1,
# meaning that all the higher-order autocorrelations are effectively explained by the lag-1 autocorrelation.

# Difference Data: If the time series is not stationary, it needs to be stationarized through differencing. 

# Differenced function # The max.d is set to 20 as limited to 2 otherwise.
ndiffs(ts_monthly_CPI, max.d = 20) 
# This suggests we difference the data by 2nd order. 

# Since there is a need to remove stationality then the series is differenced by 2nd order (difference = 2).
# The lag was set to 12 as the data is monthly.
diff_ts_monthly_CPI <- diff(ts_monthly_CPI, differences = 2, lag = 12)
default_settings <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
plot(ts_monthly_CPI, main = "Raw CPI data")

# Trend has been removed from differenced CPI
plot(diff_ts_monthly_CPI, main = "Differenced CPI data")

# Assess whether tred is still present in the data 
ndiffs(diff_ts_monthly_CPI)
adf.test(diff_ts_monthly_CPI)
# differenced data is now appears to be stationary (can reject null hypotheses as non-stationary)

# Normally in an ARIMA model, either an AR term or the MA term is used. To determine which one,
# the ACF and PACF plots are used.
# If there is a Positive autocorrelation at lag 1 in the Pacf then we use the AR model
# If there is a Negative autocorrelation at lag 1 then we use the MA model

Acf(diff_ts_monthly_CPI, main = "Autocorrelation plot for differenced CPI time series")
Pacf(diff_ts_monthly_CPI, main =  "Partial Autocorrelation plot for differenced CPI time series")

# No patterns emerging from the graphs (lags that break boundaries not mutiples of eachother) to remainder is just
# white noise. The Acf shows us that at lag = 0, the correlation is 1. This is because a series is always perfectly 
# correlated with itself. At lag = 1, the correlation is also very high. This means that CPI from one month to the 
# next month is similar (either before or after). The lag sharply decreases towards zero which indicates Auto 
# Regressive (AR). There is a big cut off after lag 1 from the PCF. This also indicates a Auto Regressive process.

# Let's look at the yearly cycles
par(mfrow = c(1,1))
default_settings <- par(no.readonly = TRUE)
plot(ts_monthly_CPI, main = "Raw time series")
y_boundary <- c(min(ts_monthly_CPI), max(ts_monthly_CPI))
# There is obviously an increasing trend in the CPI data. 

# The cycle can be printed - here it is a 12 month cycle ie. a year.
# The cycles can also be aggregated to produce a year on year trend. 
cycle(ts_monthly_CPI)
plot(aggregate(ts_monthly_CPI,FUN=mean), main = "Monthly CPI data aggregated by mean",
     xlab = "Time",
     ylab = "Percentage Change")

# A boxplot of the cycles can also show if a seasonality is present. 
boxplot(ts_monthly_CPI~cycle(ts_monthly_CPI), 
        main = "Boxplot of monthly cycles of CPI data",
        xlab = "Month",
        ylab = "Percentage Change")
# There doesn't appear to be a huge seasonality effect in the CPI data. This could be a result of the 
# data already being adjusted for seasonality before publication by CSO.

# applying additive model and seasonal swings should be similar year to year
decomposed_data <- decompose(ts_monthly_CPI, "additive") 
plot(decomposed_data)

par(mfrow = c(2,2))
plot(decomposed_data$trend, main = "Trend")
plot(decomposed_data$seasonal, main = "Season")
plot(decomposed_data$random, main = "Random")
plot(ts_monthly_CPI, main = "Actual Plot")
par(default_settings)
# Seasonality and randomness are very small with a large upward trend. 

# Can also use the stl function the check 
stl_CPI = stl(ts_monthly_CPI, "periodic")
seasonal_stl_CPI <- stl_CPI$time.series[,1]
trend_stl_CPI <- stl_CPI$time.series[,2]
random_stl_CPI <- stl_CPI$time.series[,3]

# Plotting stl results
plot(ts_monthly_CPI)
plot(as.ts(seasonal_stl_CPI))
plot(trend_stl_CPI)
plot(random_stl_CPI)
plot(stl_CPI)

ts_seasonal_adjust<-seasadj(stl_CPI)
par(mfrow = c(1,2))
# seasonal frequency set as 12 for monthly data. Seasonal plots for each year and summary seasonal plot
seasonplot(ts_seasonal_adjust, 12, col = rainbow(12), year.labels= TRUE, main = "Seasonal plot by year: CPI Data")
seasonplot(as.ts(seasonal_stl_CPI), 12, main = "Seasonal plot summary: CPI Data")
par(default_settings)
# We see that there is (albeit small) seasonality present in the CPI data. 

# ma() function can be used to smooth the time series. The 12 here means that 12 points will be taken with the 
# average used to plot the point on the graph. The 3 points would be the original point, the point immediately
# before and immediately after. The more points and averages we take the smoother the graph should be.

par(mfrow = c(2,2))
plot(ts_monthly_CPI, main = "Raw time series")

plot(ma(ts_monthly_CPI, 12), main = "simple moving averages (k=12)", 
     ylim = y_boundary)

plot(ma(ts_monthly_CPI, 20), main = "simple moving averages (k=20)", 
     ylim = y_boundary)

plot(ma(ts_monthly_CPI, 30), main = "simple moving averages (k=30)", 
     ylim = y_boundary)

# reset settings again
par(default_settings)

# Can also eradicate the trend by using the moving averages method.
# Trend is created using the simple moving averages of 30 as per above.
trend_CPI <- ma(ts_monthly_CPI, order = 30, centre = T)

# Can plot the trend compared to the original data
plot(ts_monthly_CPI, main = "Original CPI data vs. Trend")
lines(trend_CPI)

# Data withot the trend
detrend_CPI = ts_monthly_CPI - trend_CPI
# Plot of what is left after the trend is eradicated from the data (seasonality and remainder left)
plot(detrend_CPI, main = "De-Trended CPI data")


#################### Estimating the ARIMA model ############################################

# Everything above was to find the 'd' value (d is the number of nonseasonal differences needed for stationarity).
# We modify the d value to suit our original findings, d = 2.

# P = Periods to lag for eg: (if P=12 then we will use the 12 previous periods of our time series in the 
# autoregressive portion of the calculation) 
# P helps adjust the line that is being fitted to forecast the series
# d is the number of nonseasonal differences needed for stationarity, and
# q is the number of lagged forecast errors in the prediction equation. 

par(mfrow = c(1,2))
fit_diff_ts_monthly_CPI <- arima(ts_monthly_CPI, order = c(8,2,0), seasonal = c(2,0,0))
summary(fit_diff_ts_monthly_CPI)
fit_diff_ts_monthly_CPI
# A model with a lower AIC value is better than the one with a higher AIC value

Box.test(fit_diff_ts_monthly_CPI$resid, lag = 12, type = "Ljung-Box")
# null hypothesis cannoted be rejected - model must be a good fit.  

checkresiduals(fit_diff_ts_monthly_CPI)
autoplot(forecast(fit_diff_ts_monthly_CPI))
autoplot(fit_diff_ts_monthly_CPI)
# model fitted as all points are within boundaries in Inverse AR roots 
# This is expected because R ensures the fitted model is both stationary and invertible. Any roots close to the unit 
# circle may be numerically unstable, and the corresponding model will not be good for forecasting.


# Forecasting
fit_diff_ts_monthly_CPI_ar <- forecast(fit_diff_ts_monthly_CPI, h = 60)
plot(fit_diff_ts_monthly_CPI_ar,
     type="l",
     lwd=2,
     col="red",
     xlab = "Percentage Change",
     ylab = "Year")

# Printing the model will show the 80% and 95% confidence values for both high and low values
print(fit_diff_ts_monthly_CPI_ar)
write.csv(fit_diff_ts_monthly_CPI_ar, file = "PredictedCPI.csv")

##############  Auto Arima Model ###########################################
auto_arima_model_CPI <- auto.arima(ts_monthly_CPI)
auto_arima_model_CPI  

#lets run with trace to compare the information criterion
auto_arima_model_CPI <- auto.arima(ts_monthly_CPI, ic = c("aicc", "aic", "bic"), trace = TRUE, approximation=FALSE)
auto_arima_model_CPI
accuracy(auto_arima_model_CPI)
# Auto Arima only goes up to 5 orders 
box_test_2 <- Box.test(auto_arima_model_CPI$residuals, type = "Ljung-Box")
box_test_3 <- Box.test(auto_arima_model_CPI$residuals, type = "Box-Pierce")
box_test_2
box_test_3
# null hypothesis cannoted be rejected - model must be a good fit.  

checkresiduals(auto_arima_model_CPI)
autoplot(forecast(auto_arima_model_CPI))
autoplot(auto_arima_model_CPI)

# manual arima model
qqnorm(fit_diff_ts_monthly_CPI$residuals)
qqline(fit_diff_ts_monthly_CPI$residuals)
# auto arima model
qqnorm(auto_arima_model_CPI$residuals)
qqline(auto_arima_model_CPI$residuals)

CPI_forecast <- forecast(auto_arima_model_CPI, 60)
plot(CPI_forecast, xlab = "Year", ylab = "Monthly CPI")


