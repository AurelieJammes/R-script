
#######################################################
###                 Time Series                   #####
#######################################################



# Define a project called "Time Series" and store it in a file called "Modelling Techniques"

# Download and store the dataset "dayly_nb_bikes" and the script "Time Series_Script" into the file "Modelling Techniques"


# Import the dataset

bikes_data = read.csv('dayly_nb_bikes.csv', header=TRUE, stringsAsFactors=FALSE)


# Install and load usefull packages for time series analysis

install.packages("forecast")

install.packages("tseries")


library("forecast")
library("tseries")
library('ggplot2')



# Plot the time series. We are interested on two variables:
  
bikes_data$Date = as.Date(bikes_data$dteday)


#  Plot the time series

ggplot(bikes_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +   xlab("")


# Create a time series object to store the variable cnt

nb_bikes_ts = ts(bikes_data[, c('cnt')])


#  Use the function <font color=green>*tsclean()*</font> to identify and replace outliers. We take the <font color=green>*log*</font> of the time series


bikes_data$clean_nb_bikes_ts<-log(tsclean(nb_bikes_ts))
ggplot() + geom_line(data = bikes_data, aes(x = Date, y = clean_nb_bikes_ts)) + ylab('Cleaned Bicycle Count')


# To apply an ARMA model, the dataset needs to be a stationary. The *diff()* function is used to differenciate once and the result is plotted

plot(diff(bikes_data$clean_nb_bikes_ts))
abline(a = 0 , b = 0)


#  Stationary test on the differenciated database

adf.test(diff(bikes_data$clean_nb_bikes_ts), alternative = "stationary")



#  The null hypothesis of non-stationarity is rejected since <font color=green>*p-value = 0.01*</font>
  
# ACF and PACF plots for the differenced series


par(mfrow=c(1,2))
acf(diff(bikes_data$clean_nb_bikes_ts), lag.max=100 , main="" )
pacf(diff(bikes_data$clean_nb_bikes_ts), lag.max=100 , main="")



# One positive pick is observed in the ACF at h=39: the differenced time series of the daily number of bikes seems to be periodic;

#  We will look for the best model using the function <font color=green>*auto.arima()*</font>

model_fit<-auto.arima(bikes_data$clean_nb_bikes_ts, max.p = 5, max.q = 5, max.P = 5, max.Q = 5, max.d = 3, max.D = 3, seasonal=TRUE)
model_fit



#  Adequation tests of residuals

plot(model_fit$residuals, ylab = "Residuals")
abline(a=0 , b=0)


# Adequation tests of residuals: normality

par(mfrow=c(1,2))
hist(model_fit$residuals , xlab= "Residuals " , xlim=c (-2,2))
qqnorm(model_fit$residuals, main="" )
qqline(model_fit$residuals)


#  Adequation tests of residuals: white noise

par(mfrow=c(1,2))
acf(model_fit$residuals, lag.max=100 , main="" )
pacf(model_fit$residuals, lag.max=100 , main="")


#  Change of P and Q

model_fit2<-arima(bikes_data$clean_nb_bikes_ts, order=c(2,1,1), seasonal= list(order=c(0,1,2),period=39))
model_fit2


#  Adequation tests of residuals

par(mfrow=c(1,2))
acf(model_fit2$residuals, lag.max=100 , main="" )
pacf(model_fit2$residuals, lag.max=100 , main="")



#  Adequation tests of residuals

par(mfrow=c(1,2))
hist(model_fit2$residuals , xlab= "Residuals " , xlim=c (-2,2))
qqnorm(model_fit2$residuals, main="" )
qqline(model_fit2$residuals)


#  Forecasting: predict the next 30 days total rental bikes

model_fit2.predict <- forecast(model_fit2, h=30)
model_fit2.predict$mean



#  Forecasting: predict the next 30 days total rental bikes

plot(model_fit2.predict)

