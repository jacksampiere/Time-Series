########################################################################## 
# Add your code after the guidelines below.
##########################################################################
library(forecast)
library(quantmod) 
library(tseries)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(reshape2)
library(fastDummies)
library(Metrics)
library(dynlm)
library(useful)
library(readxl)
library(prophet)



### PART I ###



##
## Nonfarm payroll jobs by 50 states
##
getSymbols("ALNA", src="FRED")    # https://fred.stlouisfed.org/series/ALNA
getSymbols("AKNA", src="FRED")    # https://fred.stlouisfed.org/series/AKNA 
getSymbols("AZNA", src="FRED")    # https://fred.stlouisfed.org/series/AZNA
getSymbols("ARNA", src="FRED")    # https://fred.stlouisfed.org/series/ARNA
getSymbols("CANA", src="FRED")    # https://fred.stlouisfed.org/series/CANA
getSymbols("CONA", src="FRED")    # https://fred.stlouisfed.org/series/CONA
getSymbols("CTNA", src="FRED")    # https://fred.stlouisfed.org/series/CTNA
getSymbols("DENA", src="FRED")    # https://fred.stlouisfed.org/series/DENA
getSymbols("FLNA", src="FRED")    # https://fred.stlouisfed.org/series/FLNA
getSymbols("GANA", src="FRED")    # https://fred.stlouisfed.org/series/GANA
getSymbols("HINA", src="FRED")    # https://fred.stlouisfed.org/series/HINA
getSymbols("IDNA", src="FRED")    # https://fred.stlouisfed.org/series/IDNA
getSymbols("ILNA", src="FRED")    # https://fred.stlouisfed.org/series/ILNA
getSymbols("INNA", src="FRED")    # https://fred.stlouisfed.org/series/INNA
getSymbols("IANA", src="FRED")    # https://fred.stlouisfed.org/series/IANA
getSymbols("KSNA", src="FRED")    # https://fred.stlouisfed.org/series/KSNA
getSymbols("KYNA", src="FRED")    # https://fred.stlouisfed.org/series/KYNA
getSymbols("LANA", src="FRED")    # https://fred.stlouisfed.org/series/LANA
getSymbols("MENA", src="FRED")    # https://fred.stlouisfed.org/series/MENA
getSymbols("MDNA", src="FRED")    # https://fred.stlouisfed.org/series/MDNA
getSymbols("MANA", src="FRED")    # https://fred.stlouisfed.org/series/MANA
getSymbols("MINA", src="FRED")    # https://fred.stlouisfed.org/series/MINA
getSymbols("MNNA", src="FRED")    # https://fred.stlouisfed.org/series/MNNA
getSymbols("MSNA", src="FRED")    # https://fred.stlouisfed.org/series/MSNA
getSymbols("MONA", src="FRED")    # https://fred.stlouisfed.org/series/MONA
getSymbols("MTNA", src="FRED")    # https://fred.stlouisfed.org/series/MTNA
getSymbols("NENA", src="FRED")    # https://fred.stlouisfed.org/series/NENA
getSymbols("NVNA", src="FRED")    # https://fred.stlouisfed.org/series/NVNA
getSymbols("NHNA", src="FRED")    # https://fred.stlouisfed.org/series/NHNA
getSymbols("NJNA", src="FRED")    # https://fred.stlouisfed.org/series/NJNA
getSymbols("NMNA", src="FRED")    # https://fred.stlouisfed.org/series/NMNA
getSymbols("NYNA", src="FRED")    # https://fred.stlouisfed.org/series/NYNA
getSymbols("NCNA", src="FRED")    # https://fred.stlouisfed.org/series/NCNA
getSymbols("NDNA", src="FRED")    # https://fred.stlouisfed.org/series/NDNA
getSymbols("OHNA", src="FRED")    # https://fred.stlouisfed.org/series/OHNA
getSymbols("OKNA", src="FRED")    # https://fred.stlouisfed.org/series/OKNA
getSymbols("ORNA", src="FRED")    # https://fred.stlouisfed.org/series/ORNA
getSymbols("PANA", src="FRED")    # https://fred.stlouisfed.org/series/PANA
getSymbols("RINA", src="FRED")    # https://fred.stlouisfed.org/series/RINA
getSymbols("SCNA", src="FRED")    # https://fred.stlouisfed.org/series/SCNA
getSymbols("SDNA", src="FRED")    # https://fred.stlouisfed.org/series/SDNA
getSymbols("TNNA", src="FRED")    # https://fred.stlouisfed.org/series/TNNA
getSymbols("TXNA", src="FRED")    # https://fred.stlouisfed.org/series/TXNA
getSymbols("UTNA", src="FRED")    # https://fred.stlouisfed.org/series/UTNA
getSymbols("VTNA", src="FRED")    # https://fred.stlouisfed.org/series/VTNA
getSymbols("VANA", src="FRED")    # https://fred.stlouisfed.org/series/VANA
getSymbols("WANA", src="FRED")    # https://fred.stlouisfed.org/series/WANA
getSymbols("WVNA", src="FRED")    # https://fred.stlouisfed.org/series/WVNA
getSymbols("WINA", src="FRED")    # https://fred.stlouisfed.org/series/WINA
getSymbols("WYNA", src="FRED")    # https://fred.stlouisfed.org/series/WYNA

# join data from all states
employ = cbind(ALNA, AKNA, AZNA, ARNA, CANA, CONA, CTNA, DENA, FLNA, GANA, HINA, IDNA, ILNA,
               INNA, IANA, KSNA, KYNA, LANA , MENA, MDNA, MANA, MINA, MNNA, MSNA, MONA, MTNA,
               NENA, NVNA, NHNA, NJNA, NMNA, NYNA, NCNA, NDNA, OHNA, OKNA, ORNA, PANA, RINA,
               SCNA, SDNA, TNNA, TXNA, UTNA, VTNA, VANA, WANA, WVNA, WINA, WYNA)
# take only data after Feb. 2020
employ = employ[c(362:nrow(employ)),]
# calculate percent change between Feb. 2020 and most recent month (Oct. 2021)
employ = data.frame(employ) # convert to data frame
last = nrow(employ) # index of last row
pct_changes = ((employ[nrow(employ),] - employ[1,]) / employ[1,]) * 100 # calculate percent changes
# wrangle data for plotting purposes
pct_changes = t(pct_changes)
pct_changes = cbind('State'=rownames(pct_changes), pct_changes)
rownames(pct_changes) = 1:nrow(pct_changes)
colnames(pct_changes) = c('State', 'pct_change')
pct_changes = data.frame(pct_changes)
pct_changes$pct_change = as.numeric(pct_changes$pct_change)
pct_changes = pct_changes[order(-pct_changes$pct_change),]
# drop "NA" from state names
pct_changes$State = str_sub(pct_changes$State, 1,2)

# plot bar chart
ggplot(pct_changes, aes(x=reorder(State, -pct_change), y=pct_change)) + 
  geom_bar(stat ="identity") +
  geom_text(aes(y=pct_change+0.75*sign(pct_change), label=sprintf("%0.2f", round(pct_change, digits=2))), position=position_dodge(width=0.9), size=3.5, angle=90) +
  xlab('State') +
  ylab ('Percent Change') +
  ggtitle('Percent Change of Employment by State: Feb. 2020 to Oct. 2021') +
  theme(plot.title = element_text(hjust = 0.5))



### PART II ###



##
## Import Google Mobility Data (From https://www.google.com/covid19/mobility/ under Region CSVs)
##
google20 = read.csv("2020_US_Region_Mobility_Report.csv")
google21 = read.csv("2021_US_Region_Mobility_Report.csv")

# Convert date to time series variable
google20 = google20 %>% mutate(date = ymd(date)) %>% arrange(date)
google21 = google21 %>% mutate(date = ymd(date)) %>% arrange(date)

# Extract state data with traffic to retail and recreation places: Daily series
g.20 = subset(google20, sub_region_2 =="", select=c(sub_region_1, date, retail_and_recreation_percent_change_from_baseline))
gw.20 = spread(g.20, sub_region_1, retail_and_recreation_percent_change_from_baseline)
g.21 = subset(google21, sub_region_2 =="", select=c(sub_region_1, date, retail_and_recreation_percent_change_from_baseline))
gw.21 = spread(g.21, sub_region_1, retail_and_recreation_percent_change_from_baseline)
gw = rbind(gw.20, gw.21)

# Convert Daily series to monthly series
g.20 = g.20 %>% mutate(year_month = make_date(year=year(date),month=month(date)))
g.20m = g.20 %>% group_by(sub_region_1, year_month) %>% summarize(retail=mean(retail_and_recreation_percent_change_from_baseline))
g.21 = g.21 %>% mutate(year_month = make_date(year=year(date),month=month(date)))
g.21m = g.21 %>% group_by(sub_region_1, year_month) %>% summarize(retail=mean(retail_and_recreation_percent_change_from_baseline))
gw.20m = spread(g.20m, sub_region_1, retail)
gw.21m = spread(g.21m, sub_region_1, retail)
gwm = rbind(gw.20m, gw.21m)

# we're going to build a model that uses mobility data to predict job changes
# we'll use Feb. 2020 through July 2021 to train and Aug. + Sept. 2021 to test
# then we'll predict on Oct. 2021

# drop Nov. 2021 mobility data
gwm = gwm[-nrow(gwm),]
# we don't have "V1" and DC in employment data, so we drop them from the mobility data
gwm = gwm[, !names(gwm) %in% c("V1", "District of Columbia")]
# melt gwm
gwm_panel = melt(gwm, id.vars="year_month", measure.vars=colnames(gwm[, -which(names(gwm) == "year_month")]),
             value.name = "mobility", variable.name = "State")
# convert columns to chr for compatibility with employ_panel
gwm_panel[] <- lapply(gwm_panel, as.character)
gwm_panel$mobility = as.numeric(gwm_panel$mobility) # need jobs to still be numeric

# create year_month column for join purposes
employ$year_month = rownames(employ)
# drop index
rownames(employ) = 1:nrow(employ)
# melt employ
employ_panel = melt(employ, id.vars="year_month", measure.vars=colnames(employ[, -which(names(employ) == "year_month")]),
                 value.name = "jobs", variable.name = "State")
# drop "NA" from state names
employ_panel$State = str_sub(employ_panel$State, 1, 2)
# convert from abbreviations to full names for join purposes
employ_panel$State = state.name[match(employ_panel$State,state.abb)]

# join data frames
gwm_employ_panel = left_join(gwm_panel, employ_panel, by = c("State", "year_month"))
# convert state column to dummy variables
gwm_employ_panel = dummy_cols(gwm_employ_panel, select_columns='State')
# remove state column
gwm_employ_panel$State = NULL
# convert to date for filtering purposes
gwm_employ_panel$year_month = as.Date(gwm_employ_panel$year_month, format="%Y-%m-%d")
# split into train, test, and holdout sets
# train = Feb. 2020 - July 2021
# test = Aug. + Sept. 2021
# holdout = Oct. 2021
train = subset(gwm_employ_panel, year_month <= "2021-07-01")
test = subset(gwm_employ_panel, year_month >= "2021-08-01" & year_month <= "2021-09-01")
holdout = subset(gwm_employ_panel, year_month == "2021-10-01")

# run cross-sectional regression with all data
model_01 = lm(jobs ~ .-year_month, data=train)
summary(model_01)
# predict on test set
model_01_pred = predict(model_01, test[, !names(test) %in% c("jobs")])
# evaluate rmse on test set
model_01_rmse = rmse(test$jobs, model_01_pred)
print(model_01_rmse) # rmse = 121.2578

# add AR(1) term
cols_shift = c("jobs")
new_names = c("jobs_t-1")
gep_lagged = shift.column(data=gwm_employ_panel, columns=cols_shift, newNames=new_names, len=1, up=FALSE)
# remove Feb. 2020 since don't have Jan. 2020 mobility data
gep_lagged = subset(gep_lagged, year_month != "2020-02-01")
# new regression with lagged job data included
train_lagged = subset(gep_lagged, year_month <= "2021-07-01")
test_lagged = subset(gep_lagged, year_month >= "2021-08-01" & year_month <= "2021-09-01")
holdout_lagged = subset(gep_lagged, year_month == "2021-10-01")
model_02 = lm(jobs ~ .-year_month, data=train_lagged)
summary(model_02)
model_02_pred = predict(model_02, test_lagged[, !names(test_lagged) %in% c("jobs")])
model_02_rmse = rmse(test_lagged$jobs, model_02_pred)
print(model_02_rmse) # rmse is 92.66337

# add lagged mobility (1 month) to AR(1) model
cols_shift = c("mobility", "jobs")
new_names = c("mobility_t-1", "jobs_t-1")
gep_lagged = shift.column(data=gwm_employ_panel, columns=cols_shift, newNames=new_names, len=1, up=FALSE)
# remove Feb. 2020 since don't have Jan. 2020 mobility data
gep_lagged = subset(gep_lagged, year_month != "2020-02-01")
# new regression with lagged job and mobility data included
train_lagged = subset(gep_lagged, year_month <= "2021-07-01")
test_lagged = subset(gep_lagged, year_month >= "2021-08-01" & year_month <= "2021-09-01")
holdout_lagged = subset(gep_lagged, year_month == "2021-10-01")
model_03 = lm(jobs ~ .-year_month, data=train_lagged)
summary(model_03)
model_03_pred = predict(model_03, test_lagged[, !names(test_lagged) %in% c("jobs")])
model_03_rmse = rmse(test_lagged$jobs, model_03_pred)
print(model_03_rmse) # rmse is 92.85

# the AR(1) model with no mobility lag had the lowest rmse on the test set
# we thus use this model to predict on the Oct. 2021 data
oct_pred = predict(model_02, holdout_lagged[, !names(holdout_lagged) %in% c("jobs")])
oct_rmse = rmse(holdout_lagged$jobs, oct_pred)
print(oct_rmse) # rmse is 99.50138



### PART III ###



### HOUSING DATA ###

# read data
housing = read_excel("HPI_PO_state.xlsx")
# keep only a subset of columns
housing = housing[,-c(4,6)]
# compute growth rate
gr = diff(log(housing$index_sa))
# insert value to match length (will be removed)
gr = c(0, gr)
# create column for growth rate
housing$index_g = gr
# since we are taking the growth rate from the previous quarter
# we'll drop the rows corresponding to the first quarter in the series
housing = housing[!(housing$yr == 1991 & housing$qtr == 1),]
# we don't have job data for DC, so let's omit this data
housing = housing[housing$state != "DC",]
# rename year for future joining
names(housing)[names(housing) == "yr"] = "year"

### JOB DATA ###

# reinitialize job data frame
employ = cbind(ALNA, AKNA, AZNA, ARNA, CANA, CONA, CTNA, DENA, FLNA, GANA, HINA, IDNA, ILNA,
               INNA, IANA, KSNA, KYNA, LANA , MENA, MDNA, MANA, MINA, MNNA, MSNA, MONA, MTNA,
               NENA, NVNA, NHNA, NJNA, NMNA, NYNA, NCNA, NDNA, OHNA, OKNA, ORNA, PANA, RINA,
               SCNA, SDNA, TNNA, TXNA, UTNA, VTNA, VANA, WANA, WVNA, WINA, WYNA)
employ = data.frame(employ)
# add data column
employ$year_month = as.Date(rownames(employ), format = "%Y-%m-%d")
# convert to time series
inds = as.Date(rownames(employ), format="%Y-%m-%d")
employ.ts = xts(employ, order.by=inds)
# convert to quarterly data
employ.ts = apply.quarterly(employ.ts[, -which(names(employ) == "year_month")], mean)
# create year and quarter columns
employ.ts$year = str_sub(index(employ.ts), 1, 4)
employ.ts$qtr = str_sub(as.yearqtr(index(employ.ts)), 7, 7)
# convert back to data frame
employ = data.frame(employ.ts)
# reset index
rownames(employ) = 1:nrow(employ)
# melt
employ = melt(employ, id.vars=c("year", "qtr"), measure.vars=colnames(employ[, !names(employ) %in% c("year", "qtr")]),
              value.name = "jobs", variable.name = "state")
# drop "NA" from state names
employ$state = str_sub(employ$state, 1, 2)


### MODELING ###


# keep only time, index_g, and state for pure time series models
housing.ts = housing[,which(names(housing) %in% c("state", "year", "qtr", "index_g"))]
# sequence of quarterly dates to use
dates = seq.Date(as.Date("1991-04-01"), as.Date("2021-04-01"), by="quarter")
# for arima results
arima_rmse = c()
# for prophet results
prophet_rmse = c()

# for each state...
for (state in unique(housing.ts$state)) {

  print(state)
  
  # subset by current state
  housing.ts.curr = housing.ts[housing.ts$state == state,]
  # extract growth rates
  gr_curr = housing.ts.curr$index_g
  # create time series object
  ts.curr = xts(gr_curr, order.by=dates)
  colnames(ts.curr) = c("index_g")
  # train/test split
  train = ts.curr[1:(nrow(ts.curr)-12),] # leave 3 years for test
  test = ts.curr[(nrow(ts.curr)-11):nrow(ts.curr),] # last 3 years of data
  # fit ARIMA on train
  arima_model = auto.arima(train)
  # predict on test, compute rmse
  arima_pred = forecast(arima_model, h=12)
  plot(arima_pred)
  # store results
  arima_pred = data.frame(arima_pred)[,c("Point.Forecast")] # extract yhat
  arima_rmse = append(arima_rmse, rmse(test$index_g, arima_pred))

  # convert xts to data frame for Prophet
  ts.curr = data.frame(ts.curr)
  ts.curr$ds = rownames(ts.curr)
  # remove index and rename columns to ds/y
  colnames(ts.curr) = c("y", "ds")
  rownames(ts.curr) = 1:nrow(ts.curr)
  ts.curr = ts.curr[,c(2,1)]
  # re-split
  train = ts.curr[1:(nrow(ts.curr)-12),] # leave 3 years for test
  test = ts.curr[(nrow(ts.curr)-11):nrow(ts.curr),] # last 3 years of data
  # fit Prophet on train
  prophet_model = prophet(train, daily.seasonality=TRUE, weekly.seasonality=TRUE)
  # predict on test, compute rmse, store in list
  future = make_future_dataframe(prophet_model, freq="quarter", periods=12)
  prophet_pred_whole = predict(prophet_model, future)
  # extract forecast period
  prophet_pred = prophet_pred_whole[(nrow(prophet_pred_whole)-11):nrow(prophet_pred_whole), c("yhat")]
  # store results
  prophet_rmse = append(prophet_rmse, rmse(test$y, prophet_pred))
  
}

# determine which model performs better on average
print("ARIMA:")
print(mean(arima_rmse))

print("Prophet:")
print(mean(prophet_rmse))


# ARIMA performs better
# before adding jobs as an additional regressor, let's see what the plain ARIMA forecasts


# to store states
states = c()
# to store growth rates
grs = c()

for (state in unique(housing$state)) {
  
  # subset by current state
  housing.ts.curr = housing.ts[housing.ts$state == state,]
  # extract growth rates
  gr_curr = housing.ts.curr$index_g
  # create time series object
  ts.curr = xts(gr_curr, order.by=dates)
  colnames(ts.curr) = c("index_g")
  # fit ARIMA on entire dataset
  arima_model = auto.arima(ts.curr)
  # predict on test, compute rmse
  arima_pred = forecast(arima_model, h=12)
  plot(arima_pred)
  # store results
  arima_pred = data.frame(arima_pred)[,c("Point.Forecast")] # extract yhat
  # compute average growth rate over next 3 years
  gr_mean = mean(arima_pred)
  # store
  states = append(states, state)
  grs = append(grs, gr_mean)
  
  print(state)
  print(gr_mean)
}

# create data frame of mean growth rates
preds = data.frame(states, grs)
tail(preds)
# plot an ordered bar chart
ggplot(preds, aes(x=reorder(states, -grs), y=grs)) + 
  geom_bar(stat ="identity") +
  geom_text(aes(y=grs+0.005*sign(grs), label=sprintf("%0.2f", round(grs, digits=2))), position=position_dodge(width=0.9), size=3.5, angle=90) +
  xlab('State') +
  ylab ('Predicted Avg. Growth Rate (%)') +
  ggtitle('Forecasted Housing Price Growth Rates by State (ARIMA)') +
  theme(plot.title = element_text(hjust = 0.5))


# now, let's try ARIMA with jobs as an additional regressor


# join job and housing data
housingj = left_join(housing, employ, by=c("year", "qtr", "state"))
# to check if jobs improves the forecast quality
rmse_vals = c()

for (state in unique(housingj$state)) {
  
  housing_curr = housingj[housingj$state == state,]
  housing_curr = housing_curr[,!names(housing_curr) %in% c("index_sa")] # drop index_sa
  housing_curr = housing_curr[,c("index_g", "jobs")]
  housing_curr.ts = xts(housing_curr, order.by=dates)
  
  train = housing_curr.ts[1:(nrow(housing_curr.ts)-12),] # leave 3 years for test
  test = housing_curr.ts[(nrow(housing_curr.ts)-11):nrow(housing_curr.ts),] # last 3 years of data
  
  fit = auto.arima(train[,1], xreg=as.matrix(train[,2]))
  
  fc_jobs = forecast(train[,2], h=12)
  fc_jobs = as.matrix(fc_jobs$mean)
  
  pred = forecast(fit, xreg=fc_jobs)
  plot(pred)
  
  pred = data.frame(pred)[,c("Point.Forecast")]
  
  rmse_vals = append(rmse_vals, rmse(test$index_g, pred))
  
}

# check mean rmse
print("ARIMA with jobs:")
print(mean(rmse_vals)) # slight improvement



# the ARIMA + jobs model performs the best on the test set,
# so we use this model for our final forecast


# for plotting
states = c()
grs = c()

for (state in unique(housingj$state)) {
  
  # subset by current state
  housing_curr = housingj[housingj$state == state,]
  # extract growth rates
  xy_curr = housing_curr[,c("index_g", "jobs")]
  # create time series object
  ts.curr = xts(xy_curr, order.by=dates)
  # fit ARIMA on entire dataset
  fit = auto.arima(ts.curr[,1], xreg=as.matrix(ts.curr[,2]))
  fc_jobs = forecast(ts.curr[,2], h=12)
  fc_jobs = as.matrix(fc_jobs$mean)
  # predict on test, compute rmse
  pred = forecast(fit, xreg=fc_jobs)
  plot(pred)
  
  pred = data.frame(pred)[,c("Point.Forecast")]
  
  gr_mean = mean(pred)
  states = append(states, state)
  grs = append(grs, gr_mean)
  
  print(state)
  print(gr_mean)
  
}

# create data frame of mean growth rates
preds_ex = data.frame(states, grs)
tail(preds_ex)
# plot an ordered bar chart
ggplot(preds_ex, aes(x=reorder(states, -grs), y=grs)) + 
  geom_bar(stat ="identity") +
  geom_text(aes(y=grs+0.005*sign(grs), label=sprintf("%0.2f", round(grs, digits=2))), position=position_dodge(width=0.9), size=3.5, angle=90) +
  xlab('State') +
  ylab ('Predicted Avg. Growth Rate (%)') +
  ggtitle('Forecasted Housing Price Growth Rates by State (ARIMA + Jobs)') +
  theme(plot.title = element_text(hjust = 0.5))










