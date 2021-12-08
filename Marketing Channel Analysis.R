# imports
library(readxl)
library(reshape2)
library(ggplot2)
library(plotrix)
library(xts)
library(dynlm)
library(forecast)

# read data
tv = read_excel('TVlift.xlsx')

# let's overlay the different variables on a plot
# melt data frame into long format
tv.melt <- melt(tv,  id.vars = 'time', variable.name = 'series')
# create line plot for each column in data frame
ggplot(tv.melt, aes(time, value)) +
  geom_line(aes(colour=series))

# to get a closer look at each variable, we use 2 y-axes
# we repeat this for each available predictor
twoord.plot(lx=1:nrow(tv), ly=tv$trans, rx=1:nrow(tv), ry=tv$snb, type='l', lcol='red',
            rcol='blue', ylab='trans', rylab='snb', main='trans and snb over time', xlab='Time Index',
            xticklab=tv$time, rytickpos=c(0,15000,30000,45000, 60000))

twoord.plot(lx=1:nrow(tv), ly=tv$trans, rx=1:nrow(tv), ry=tv$sb, type='l', lcol='red',
            rcol='blue', ylab='trans', rylab='sb', main='trans and sb over time', xlab='Time Index',
            xticklab=tv$time)

tv.sub = tv[200:nrow(tv),] # tv is 0 up until near the end of the data
twoord.plot(lx=1:nrow(tv.sub), ly=tv.sub$trans, rx=1:nrow(tv.sub), ry=tv.sub$tv, type='l', lcol='red',
            rcol='blue', ylab='trans', rylab='tv', main='trans and tv over time', xlab='Time Index',
            xticklab=tv.sub$time)

# extract time column to use as index
inds = tv$time
# create xts object
tv.ts = xts(tv[,c(2:ncol(tv))], order.by=inds)
head(tv.ts)

# let's first plot the response variable
plot(tv.ts$trans, ylab='Sales')

# baseline cross-sectional regression
baseline = lm(trans ~ sb + snb + tv, data=tv.ts)
summary(baseline)

# lagged model
lagged = dynlm(trans ~ L(sb, 90) + snb + L(tv,30), data=tv.ts)
summary(lagged)

# one could argue that the initial data is not stationary
# in this case, we take the growth rate of each column
tv.ts$sb_g = diff(log(tv.ts$sb))
tv.ts$snb_g = diff(log(tv.ts$snb))
tv.ts$tv_g = diff(log(tv.ts$tv))
tv.ts$trans_g = diff(log(tv.ts$trans))
head(tv.ts)

# plot growth rate of response variable
plot(tv.ts$trans_g, ylab='Growth Rate of Sales')
# the data seems more stationary now
ggAcf(tv.ts$trans_g, lag=60)

# then proceed with modeling...

# building a model that uses the growth rate is difficult in this case
# this is because there are a lot of zeros in the tv and sb columns
# and the log turns these zeros into invalid values
# hence why we used the original data for the above models
