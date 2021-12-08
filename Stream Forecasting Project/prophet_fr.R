# imports
library(dplyr)
library(prophet)
library(Metrics)
library(xts)
library(ggplot2)



### HELPER FUNCTIONS ###

# function to aggregate streams across top 10, 50, and 200 tracks
# and format for prophet
agg_prophet = function(df) {
  # df  = data frame of spotify top 200 tracks over time
  # returns aggregated prophet data frame for top 10, top 50, and top 200 tracks
  
  # for storing output data frames
  out = list()
  # keep only necessary columns
  df = df[, c('date', 'Position', 'Streams')]
  
  # subset data
  
  # top 10
  df.10 = df[df$Position <= 10,]
  df.10 = df.10 %>% group_by(date) %>% summarize(streams=sum(Streams))
  df.10 = data.frame(df.10)
  colnames(df.10) = c('ds', 'y')
  # top 50
  df.50 = df[df$Position <= 50,]
  df.50 = df.50 %>% group_by(date) %>% summarize(streams=sum(Streams))
  df.50 = data.frame(df.50)
  colnames(df.50) = c('ds', 'y')
  # top 200 (all available tracks)
  df.200 = df %>% group_by(date) %>% summarize(streams=sum(Streams))
  df.200 = data.frame(df.200)
  colnames(df.200) = c('ds', 'y')
  
  # store in list
  out$df.10 = df.10
  out$df.50 = df.50
  out$df.200 = df.200
  
  return(out)
  
}

# function to train/val/test split
split = function(ts, val_num, test_num) {
  # ts = time series of aggregated data
  # val_num, test_num = number of samples for validation and testing
  
  # for storing output time series
  out = list()
  # length of time series
  n = nrow(ts)
  # indices for slicing
  test_start = n - test_num + 1
  val_end = test_start - 1
  val_start = val_end - val_num + 1
  # create training, validation, and test sets
  train = ts[1:val_start-1,]
  val = ts[val_start:val_end,]
  test = ts[test_start:n,]
  
  # store in list
  out$train = train
  out$val = val
  out$test = test
  
  return(out)
  
}

### END ###



# read data
df = read.csv('spotifycharts_20170629to20180126_fr.csv') # Australia

# aggregate data and format for prophet
df.10 = agg_prophet(df)$df.10
df.50 = agg_prophet(df)$df.50
df.200 = agg_prophet(df)$df.200


# take only data before December (difficult to account for holiday behavior with the amount of data we have)
df.10 = df.10[1:156,]
df.50 = df.50[1:156,]
df.200 = df.200[1:156,]


# configure validation and test set sizes (2 weeks each)
val.size = 14
test.size = 14

# split data

# top 10 streams
train.10 = split(df.10, val.size, test.size)$train
val.10 = split(df.10, val.size, test.size)$val
test.10 = split(df.10, val.size, test.size)$test

# top 50 streams
train.50 = split(df.50, val.size, test.size)$train
val.50 = split(df.50, val.size, test.size)$val
test.50 = split(df.50, val.size, test.size)$test

# top 10 streams
train.200 = split(df.200, val.size, test.size)$train
val.200 = split(df.200, val.size, test.size)$val
test.200 = split(df.200, val.size, test.size)$test



### TOP 10 MODEL ###

# train model
m.10 = prophet(holidays=NULL) # tweak daily/yearly seasonality, changepoint.prior.scale, etc.
m.10 = add_country_holidays(m.10, country_name = 'FR')
m.10 = fit.prophet(m.10, train.10)
future.10 = make_future_dataframe(m.10, periods=val.size)
forecast.10 = predict(m.10, future.10)
plot(m.10, forecast.10)

# evaluate validation set performance
val.pred.10 = tail(forecast, val.size)$yhat # get samples in validation set
print(rmse(val.10$y, val.pred.10))
print(mape(val.10$y, val.pred.10)*100)

# re-train on entire training set
train.10.full = rbind(train.10, val.10)
m.10 = prophet(holidays=NULL)
m.10 = add_country_holidays(m.10, country_name = 'SE')
m.10 = fit.prophet(m.10, train.10.full)
future.10 = make_future_dataframe(m.10, periods=test.size)
forecast.10 = predict(m.10, future.10)
plot(m.10, forecast.10)

# evaluate test set performance
test.pred.10 = tail(forecast.10, test.size)$yhat # get samples in validation set
print(rmse(test.10$y, test.pred.10))
print(mape(test.10$y, test.pred.10)*100)

# plot test set performance
plot.10 = data.frame(test.10, test.pred.10)
plot.10 = xts(plot.10[,-1], order.by=as.POSIXct(plot.10$ds))
colnames(plot.10) = c('y', 'yhat')
ggplot(plot.10, aes(index(plot.10))) +
  geom_line(aes(y=y, color="True Values"), size=1.5) +
  geom_line(aes(y=yhat, color="Forecast"), size=1.5) + 
  ggtitle("France Top 10") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(gb)[c(T,F)])



### TOP 50 MODEL ###

# train model
m.50 = prophet(holidays=NULL) # tweak daily/yearly seasonality, changepoint.prior.scale, etc.
m.50 = add_country_holidays(m.50, country_name = 'FR')
m.50 = fit.prophet(m.50, train.50)
future.50 = make_future_dataframe(m.50, periods=val.size)
forecast.50 = predict(m.50, future.50)
plot(m.50, forecast.50)

# evaluate validation set performance
val.pred.50 = tail(forecast, val.size)$yhat # get samples in validation set
print(rmse(val.50$y, val.pred.50))
print(mape(val.50$y, val.pred.50)*100)

# re-train on entire training set
train.50.full = rbind(train.50, val.50)
m.50 = prophet(holidays=NULL)
m.50 = add_country_holidays(m.50, country_name = 'SE')
m.50 = fit.prophet(m.50, train.50.full)
future.50 = make_future_dataframe(m.50, periods=test.size)
forecast.50 = predict(m.50, future.50)
plot(m.50, forecast.50)

# evaluate test set performance
test.pred.50 = tail(forecast.50, test.size)$yhat # get samples in validation set
print(rmse(test.50$y, test.pred.50))
print(mape(test.50$y, test.pred.50)*100)

# plot test set performance
plot.50 = data.frame(test.50, test.pred.50)
plot.50 = xts(plot.50[,-1], order.by=as.POSIXct(plot.50$ds))
colnames(plot.50) = c('y', 'yhat')
ggplot(plot.50, aes(index(plot.50))) +
  geom_line(aes(y=y, color="True Values"), size=1.5) +
  geom_line(aes(y=yhat, color="Forecast"), size=1.5) + 
  ggtitle("France Top 50") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(gb)[c(T,F)])



### TOP 200 MODEL ###

# train model
m.200 = prophet(holidays=NULL) # tweak daily/yearly seasonality, changepoint.prior.scale, etc.
m.200 = add_country_holidays(m.200, country_name = 'FR')
m.200 = fit.prophet(m.200, train.200)
future.200 = make_future_dataframe(m.200, periods=val.size)
forecast.200 = predict(m.200, future.200)
plot(m.200, forecast.200)

# evaluate validation set performance
val.pred.200 = tail(forecast, val.size)$yhat # get samples in validation set
print(rmse(val.200$y, val.pred.200))
print(mape(val.200$y, val.pred.200)*100)

# re-train on entire training set
train.200.full = rbind(train.200, val.200)
m.200 = prophet(holidays=NULL)
m.200 = add_country_holidays(m.200, country_name = 'FR')
m.200 = fit.prophet(m.200, train.200.full)
future.200 = make_future_dataframe(m.200, periods=test.size)
forecast.200 = predict(m.200, future.200)
plot(m.200, forecast.200)

# evaluate test set performance
test.pred.200 = tail(forecast.200, test.size)$yhat # get samples in validation set
print(rmse(test.200$y, test.pred.200))
print(mape(test.200$y, test.pred.200)*100)

# plot test set performance
plot.200 = data.frame(test.200, test.pred.200)
plot.200 = xts(plot.200[,-1], order.by=as.POSIXct(plot.200$ds))
colnames(plot.200) = c('y', 'yhat')
ggplot(plot.200, aes(index(plot.200))) +
  geom_line(aes(y=y, color="True Values"), size=1.5) +
  geom_line(aes(y=yhat, color="Forecast"), size=1.5) + 
  ggtitle("France Top 200") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(gb)[c(T,F)])


