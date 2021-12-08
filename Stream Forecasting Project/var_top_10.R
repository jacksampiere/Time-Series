# imports
library(dplyr)
library(glue)
library(xts)
library(vars)
library(Metrics)
library(ggplot2)
library(bayesforecast)



### HELPER FUNCTIONS ###

# function to aggregate streams across top 10, 50, and 200 tracks
agg = function(df, pref) {
  # df  = data frame of spotify top 200 tracks over time
  # pref = prefix for column naming
  # returns aggregated time series for top 10, top 50, and top 200 tracks
  
  # for storing output data frames
  out = list()
  # keep only necessary columns
  df = df[, c('date', 'Position', 'Streams')]
  
  # subset data
  
  # top 10
  df.10 = df[df$Position <= 10,]
  df.10 = df.10 %>% group_by(date) %>% summarize(streams=sum(Streams))
  colnames(df.10) = c('date', glue('{pref}_10'))
  ts.10 = xts(df.10[,-1], order.by=as.POSIXct(df.10$date))
  
  # top 50
  df.50 = df[df$Position <= 50,]
  df.50 = df.50 %>% group_by(date) %>% summarize(streams=sum(Streams))
  colnames(df.50) = c('date', glue('{pref}_50'))
  ts.50 = xts(df.50[,-1], order.by=as.POSIXct(df.50$date))
  # top 200 (all available tracks)
  df.200 = df %>% group_by(date) %>% summarize(streams=sum(Streams))
  colnames(df.200) = c('date', glue('{pref}_200'))
  ts.200 = xts(df.200[,-1], order.by=as.POSIXct(df.200$date))
  
  # store in list
  out$ts.10 = ts.10
  out$ts.50 = ts.50
  out$ts.200 = ts.200
  
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
df.de = read.csv('spotifycharts_20170629to20180126_de.csv') # Denmark
df.fr = read.csv('spotifycharts_20170629to20180126_fr.csv') # France
df.gb = read.csv('spotifycharts_20170629to20180126_gb.csv') # England
df.nl = read.csv('spotifycharts_20170629to20180126_nl.csv') # Netherlands
df.se = read.csv('spotifycharts_20170629to20180126_se.csv') # Sweden
df.au = read.csv('spotifycharts_20170629to20180126_au.csv') # Australia
df.br = read.csv('spotifycharts_20170629to20180126_br.csv') # Brazil
df.ca = read.csv('spotifycharts_20170629to20180126_ca.csv') # Canada
df.mx = read.csv('spotifycharts_20170629to20180126_mx.csv') # Mexico
df.us = read.csv('spotifycharts_20170629to20180126_us.csv') # US




# aggregate data and convert to time series

# Denmark
ts.de.10 = agg(df.de, 'de')$ts.10
# France
ts.fr.10 = agg(df.fr, 'fr')$ts.10
# England
ts.gb.10 = agg(df.gb, 'gb')$ts.10
# Netherlands
ts.nl.10 = agg(df.nl, 'nl')$ts.10
# Sweden
ts.se.10 = agg(df.se, 'se')$ts.10
# Australia
ts.au.10 = agg(df.au, 'au')$ts.10
# Brazil
ts.br.10 = agg(df.br, 'br')$ts.10
# Canada
ts.ca.10 = agg(df.ca, 'ca')$ts.10
# Mexico
ts.mx.10 = agg(df.mx, 'mx')$ts.10
# US
ts.us.10 = agg(df.us, 'us')$ts.10


# create time series for each bin
ts.10 = cbind(ts.de.10, ts.fr.10, ts.gb.10, ts.nl.10, ts.se.10, ts.au.10,
              ts.br.10, ts.ca.10, ts.mx.10, ts.us.10)

# take only data before December (can't account for holiday behavior with the amount of data we have)
ts.10 = ts.10[1:156,]

# examine cross ACF for all time series
acf(ts.10)

# group time series based on cross ACF
ts.var.1 = ts.10[,c('gb_10', 'ca_10', 'us_10', 'mx_10', 'br_10')]
ts.var.2 = ts.10[,c('de_10', 'nl_10', 'gb_10')]

# clean up data for plotting
plot.1 = ts.10[,c('gb_10', 'ca_10', 'us_10', 'mx_10', 'br_10')]
colnames(plot.1) = c('gb', 'ca', 'us', 'mx', 'br')
plot.2 = ts.10[,c('de_10', 'nl_10', 'gb_10')]
colnames(plot.2) = c('de', 'nl', 'gb')
plot.3 = ts.10[,c('au_10', 'fr_10')]
colnames(plot.3) = c('au', 'fr')

# create ACF plots for sub-series
ggacf(plot.1, title='ACF, VAR Model 1') + theme(plot.title = element_text(hjust = 0.5))
ggacf(plot.2, title='ACF, VAR Model 2') + theme(plot.title = element_text(hjust = 0.5))
# plot Australia
ggplot(plot.3, aes(index(plot.3))) +
  geom_line(aes(y=au), size=1.5) +
  ggtitle('Stream Volumes: Australia') +
  xlab('Date') + ylab('Streams') +
  theme(plot.title = element_text(hjust = 0.5, size=20))
# plot France
ggplot(plot.3, aes(index(plot.3))) +
  geom_line(aes(y=fr), size=1.5) +
  ggtitle('Stream Volumes: France') +
  xlab('Date') + ylab('Streams') +
  theme(plot.title = element_text(hjust = 0.5, size=20))

# configure validation and test sizes
val.size = 14 # 2 weeks
test.size = 14 # 2 weeks

# split data for VAR 1
train.var.1 = split(ts.var.1, val.size, test.size)$train
val.var.1 = split(ts.var.1, val.size, test.size)$val
test.var.1 = split(ts.var.1, val.size, test.size)$test
# split data for VAR 2
train.var.2 = split(ts.var.2, val.size, test.size)$train
val.var.2 = split(ts.var.2, val.size, test.size)$val
test.var.2 = split(ts.var.2, val.size, test.size)$test



# EDA for each dataset

# VAR 1
par(mfrow=c(3,1))
plot(train.var.1)
plot(val.var.1)
plot(test.var.1)
# VAR 2
plot(train.var.2)
plot(val.var.2)
plot(test.var.2)
par(mfrow=c(1,1))



### MODELING: VAR 1 ###

# number of correlated variables to use in VAR model
n.vars.1 = 5
# specify which lags to use (tune with validation set)
lags.1 = c(1,2,7,8,9) # chosen based on ACF plots / tuning
# length of lag vector for each regressor (add 1 for constant)
size.1 = n.vars.1 * max(lags.1) + 1
# initialize restriction vector with all zeros
restrict.vec = integer(size.1)

# insert specified lags to keep at indices
for (lag in lags.1) {
  # create a vector of indices at which to insert ones with the current lag values
  insert.start = n.vars.1*lag - (n.vars.1-1)
  insert.end = n.vars.1*lag
  # modify restriction vector
  restrict.vec[insert.start:insert.end] = 1
}

# expand restriction vector and convert to matrix
restrict.matrix.1 = matrix(rep(restrict.vec, times=n.vars.1), nrow=n.vars.1, byrow=T)
# train model and summarize
var.1 = VAR(train.var.1, p=max(lags.1), type='const')
var.1.restr = restrict(var.1, method='man', resmat=restrict.matrix.1)
summary(var.1.restr)
# predict on validation set
pred.var.1 = predict(var.1.restr, n.ahead=val.size)
plot(pred.var.1)
# extract predictions
pred.val.gb = pred.var.1$fcst$gb_10[1:14]
pred.val.mx = pred.var.1$fcst$mx_10[1:14]
pred.val.ca = pred.var.1$fcst$ca_10[1:14]
pred.val.br = pred.var.1$fcst$br_10[1:14]
pred.val.us = pred.var.1$fcst$us_10[1:14]

# evaluate validation set performance
rmse.gb = rmse(val.var.1$gb_10, pred.val.gb)
mape.gb = mape(val.var.1$gb_10, pred.val.gb)
print(rmse.gb)
print(mape.gb*100)

rmse.mx = rmse(val.var.1$mx_10, pred.val.mx)
mape.mx = mape(val.var.1$mx_10, pred.val.mx)
print(rmse.mx)
print(mape.mx*100)

rmse.ca = rmse(val.var.1$ca_10, pred.val.ca)
mape.ca = mape(val.var.1$ca_10, pred.val.ca)
print(rmse.ca)
print(mape.ca*100)

rmse.br = rmse(val.var.1$br_10, pred.val.br)
mape.br = mape(val.var.1$br_10, pred.val.br)
print(rmse.br)
print(mape.br*100)

rmse.us = rmse(val.var.1$us_10, pred.val.us)
mape.us = mape(val.var.1$us_10, pred.val.us)
print(rmse.us)
print(mape.us*100)

print(mean(c(rmse.gb, rmse.mx, rmse.ca, rmse.br, rmse.us)))
print(mean(c(mape.gb, mape.mx, mape.ca, mape.br, mape.us)))
# add predictions to validation set
val.var.1$gb_pred = pred.val.gb
val.var.1$mx_pred = pred.val.mx
val.var.1$ca_pred = pred.val.ca
val.var.1$br_pred = pred.val.br
val.var.1$us_pred = pred.val.us
# subset validation data
gb = val.var.1[,c('gb_10','gb_pred')]
mx = val.var.1[,c('mx_10','mx_pred')]
ca = val.var.1[,c('ca_10','ca_pred')]
br = val.var.1[,c('br_10','br_pred')]
us = val.var.1[,c('us_10','us_pred')]
# plot forecasts
ggplot(gb, aes(index(gb))) +
  geom_line(aes(y=gb_10, color="True Values"), size=1.5) +
  geom_line(aes(y=gb_pred, color="Forecast"), size=1.5) + 
  ggtitle("Forecast of Stream Volume of Top 10 Tracks, Great Britain") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(gb)[c(T,F)])

ggplot(mx, aes(index(mx))) +
  geom_line(aes(y=mx_10, color="True Values"), size=1.5) +
  geom_line(aes(y=mx_pred, color="Forecast"), size=1.5) + 
  ggtitle("Forecast of Stream Volume of Top 10 Tracks, Mexico") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(mx)[c(T,F)])

ggplot(ca, aes(index(ca))) +
  geom_line(aes(y=ca_10, color="True Values"), size=1.5) +
  geom_line(aes(y=ca_pred, color="Forecast"), size=1.5) + 
  ggtitle("Forecast of Stream Volume of Top 10 Tracks, Canada") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) + 
  scale_x_continuous(breaks=index(ca)[c(T,F)])

ggplot(br, aes(index(br))) +
  geom_line(aes(y=br_10, color="True Values"), size=1.5) +
  geom_line(aes(y=br_pred, color="Forecast"), size=1.5) + 
  ggtitle("Forecast of Stream Volume of Top 10 Tracks, Brazil") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) + 
  scale_x_continuous(breaks=index(br)[c(T,F)])

ggplot(us, aes(index(us))) +
  geom_line(aes(y=us_10, color="True Values"), size=1.5) +
  geom_line(aes(y=us_pred, color="Forecast"), size=1.5) + 
  ggtitle("Forecast of Stream Volume of Top 10 Tracks, US") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) + 
  scale_x_continuous(breaks=index(us)[c(T,F)])



### TEST SET PREDICTIONS: VAR 1 ###

# redefine val.var.1 to get rid of validation predictions
val.var.1 = split(ts.var.1, val.size, test.size)$val
train.var.1.full = rbind(train.var.1, val.var.1)
# re-train VAR 1 with same lags that were optimized with validation set
var.1 = VAR(train.var.1.full, p=max(lags.1), type='const')
var.1.restr = restrict(var.1, method='man', resmat=restrict.matrix.1)
summary(var.1.restr)
# predict on test set
pred.var.1 = predict(var.1.restr, n.ahead=test.size)
plot(pred.var.1)
# extract predictions
pred.test.gb = pred.var.1$fcst$gb_10[1:14]
pred.test.mx = pred.var.1$fcst$mx_10[1:14]
pred.test.ca = pred.var.1$fcst$ca_10[1:14]
pred.test.br = pred.var.1$fcst$br_10[1:14]
pred.test.us = pred.var.1$fcst$us_10[1:14]

# evaluate test set performance
rmse.gb = rmse(test.var.1$gb_10, pred.test.gb)
mape.gb = mape(test.var.1$gb_10, pred.test.gb)
print(rmse.gb)
print(mape.gb*100)

rmse.mx = rmse(test.var.1$mx_10, pred.test.mx)
mape.mx = mape(test.var.1$mx_10, pred.test.mx)
print(rmse.mx)
print(mape.mx*100)

rmse.ca = rmse(test.var.1$ca_10, pred.test.ca)
mape.ca = mape(test.var.1$ca_10, pred.test.ca)
print(rmse.ca)
print(mape.ca*100)

rmse.br = rmse(test.var.1$br_10, pred.test.br)
mape.br = mape(test.var.1$br_10, pred.test.br)
print(rmse.br)
print(mape.br*100)

rmse.us = rmse(test.var.1$us_10, pred.test.us)
mape.us = mape(test.var.1$us_10, pred.test.us)
print(rmse.us)
print(mape.us*100)

print(mean(c(rmse.gb, rmse.mx, rmse.ca, rmse.br, rmse.us)))
print(mean(c(mape.gb, mape.mx, mape.ca, mape.br, mape.us)))
# add predictions to test set
test.var.1$gb_pred = pred.test.gb
test.var.1$mx_pred = pred.test.mx
test.var.1$ca_pred = pred.test.ca
test.var.1$br_pred = pred.test.br
test.var.1$us_pred = pred.test.us
# subset test data
gb = test.var.1[,c('gb_10','gb_pred')]
mx = test.var.1[,c('mx_10','mx_pred')]
ca = test.var.1[,c('ca_10','ca_pred')]
br = test.var.1[,c('br_10','br_pred')]
us = test.var.1[,c('us_10','us_pred')]
# plot forecasts
ggplot(gb, aes(index(gb))) +
  geom_line(aes(y=gb_10, color="True Values"), size=1.5) +
  geom_line(aes(y=gb_pred, color="Forecast"), size=1.5) + 
  ggtitle("Great Britain Top 10") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(gb)[c(T,F)])

ggplot(mx, aes(index(mx))) +
  geom_line(aes(y=mx_10, color="True Values"), size=1.5) +
  geom_line(aes(y=mx_pred, color="Forecast"), size=1.5) + 
  ggtitle("Mexico Top 10") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(mx)[c(T,F)])

ggplot(ca, aes(index(ca))) +
  geom_line(aes(y=ca_10, color="True Values"), size=1.5) +
  geom_line(aes(y=ca_pred, color="Forecast"), size=1.5) + 
  ggtitle("Canada Top 10") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) + 
  scale_x_continuous(breaks=index(ca)[c(T,F)])

ggplot(br, aes(index(br))) +
  geom_line(aes(y=br_10, color="True Values"), size=1.5) +
  geom_line(aes(y=br_pred, color="Forecast"), size=1.5) + 
  ggtitle("Brazil Top 10") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) + 
  scale_x_continuous(breaks=index(br)[c(T,F)])

ggplot(us, aes(index(us))) +
  geom_line(aes(y=us_10, color="True Values"), size=1.5) +
  geom_line(aes(y=us_pred, color="Forecast"), size=1.5) + 
  ggtitle("US Top 10") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) + 
  scale_x_continuous(breaks=index(us)[c(T,F)])



### MODELING: VAR 2 ###

# number of correlated variables to use in VAR model
n.vars.2 = 3
# specify which lags to use (tune with validation set)
lags.2 = c(1,2,7,8) # chosen based on ACF plots / tuning
# length of lag vector for each regressor (add 1 for constant)
size.2 = n.vars.2 * max(lags.2) + 1
# initialize restriction vector with all zeros
restrict.vec = integer(size.2)

# insert specified lags to keep at indices
for (lag in lags.2) {
  # create a vector of indices at which to insert ones with the current lag values
  insert.start = n.vars.2*lag - (n.vars.1-2)
  insert.end = n.vars.2*lag
  # modify restriction vector
  restrict.vec[insert.start:insert.end] = 1
}

# expand restriction vector and convert to matrix
restrict.matrix.2 = matrix(rep(restrict.vec, times=n.vars.2), nrow=n.vars.2, byrow=T)
# train model and summarize
var.2 = VAR(train.var.2, p=max(lags.2), type='const')
var.2.restr = restrict(var.2, method='man', resmat=restrict.matrix.2)
summary(var.2.restr)
# predict on validation set (only predict on de and nl)
pred.var.2 = predict(var.2.restr, n.ahead=val.size)
plot(pred.var.2)
# extract predictions
pred.val.de = pred.var.2$fcst$gb_10[1:14]
pred.val.nl = pred.var.2$fcst$nl_10[1:14]

# evaluate validation set performance
rmse.de = rmse(val.var.2$de_10, pred.val.de)
mape.de = mape(val.var.2$de_10, pred.val.de)
print(rmse.de)
print(mape.de*100)

rmse.nl = rmse(val.var.2$nl_10, pred.val.nl)
mape.nl = mape(val.var.2$nl_10, pred.val.nl)
print(rmse.nl)
print(mape.nl*100)


print(mean(c(rmse.de, rmse.nl)))
print(mean(c(mape.de, mape.nl)))
# add predictions to validation set
val.var.2$de_pred = pred.val.de
val.var.2$nl_pred = pred.val.nl
# subset validation data
de = val.var.2[,c('de_10','de_pred')]
nl = val.var.2[,c('nl_10','nl_pred')]
# plot forecasts
ggplot(de, aes(index(de))) +
  geom_line(aes(y=de_10, color="True Values"), size=1.5) +
  geom_line(aes(y=de_pred, color="Forecast"), size=1.5) + 
  ggtitle("Forecast of Stream Volume of Top 10 Tracks, Denmark") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(de)[c(T,F)])

ggplot(nl, aes(index(nl))) +
  geom_line(aes(y=nl_10, color="True Values"), size=1.5) +
  geom_line(aes(y=nl_pred, color="Forecast"), size=1.5) + 
  ggtitle("Forecast of Stream Volume of Top 10 Tracks, Netherlands") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(nl)[c(T,F)])



### TEST SET PREDICTIONS: VAR 2 ###

# redefine val.var.2 to get rid of validation predictions
val.var.2 = split(ts.var.2, val.size, test.size)$val
train.var.2.full = rbind(train.var.2, val.var.2)
# re-train VAR 2 with same lags that were optimized with validation set
var.2 = VAR(train.var.2.full, p=max(lags.2), type='const')
var.2.restr = restrict(var.2, method='man', resmat=restrict.matrix.2)
summary(var.2.restr)
# predict on test set
pred.var.2 = predict(var.2.restr, n.ahead=test.size)
plot(pred.var.2)
# extract predictions
pred.test.de = pred.var.2$fcst$de_10[1:14]
pred.test.nl = pred.var.2$fcst$nl_10[1:14]

# evaluate test set performance
rmse.de = rmse(test.var.2$de_10, pred.test.de)
mape.de = mape(test.var.2$de_10, pred.test.de)
print(rmse.de) # 134053.6 without last day
print(mape.de*100) # 4.39% without last day

rmse.nl = rmse(test.var.2$nl_10, pred.test.nl)
mape.nl = mape(test.var.2$nl_10, pred.test.nl)
print(rmse.nl)
print(mape.nl*100)

print(mean(c(rmse.de, rmse.nl)))
print(mean(c(mape.de, mape.nl)))
# add predictions to test set
test.var.2$de_pred = pred.test.de
test.var.2$nl_pred = pred.test.nl
# subset test data
de = test.var.2[,c('de_10','de_pred')]
nl = test.var.2[,c('nl_10','nl_pred')]
# plot forecasts
ggplot(de, aes(index(de))) +
  geom_line(aes(y=de_10, color="True Values"), size=1.5) +
  geom_line(aes(y=de_pred, color="Forecast"), size=1.5) + 
  ggtitle("Denmark Top 10") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(de)[c(T,F)])

ggplot(nl, aes(index(nl))) +
  geom_line(aes(y=nl_10, color="True Values"), size=1.5) +
  geom_line(aes(y=nl_pred, color="Forecast"), size=1.5) + 
  ggtitle("Netherlands Top 10") +
  xlab('Date') + ylab('Streams') + 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title=element_blank()) +
  scale_x_continuous(breaks=index(nl)[c(T,F)])


