library(readxl)
library(broom)
library(knitr)
library(ggplot2)
library(glue)

# data frame with all countries
country = read_excel("CountryData.xlsx")
head(country)

# data frame without China
wo_china = country[country$country != 'China',]



### CROSS-SECTIONAL REGRESSIONS ###

eq1 = lm(rgdpg ~ econg + co2g + eximg, data=wo_china)
eq1_disp = tidy(eq1)
# formatting regression output
cat('\nEq1. Benchmark Equation: Pool OLS Model without China'); kable(eq1_disp, 'simple', digits=3); cat('\n')


eq2 = lm(rgdpg ~ econg + co2g + eximg, data=country)
eq2_disp = tidy(eq2)
cat('\nEq2. Pool OLS Model with China'); kable(eq2_disp, 'simple', digits=3); cat('\n')

eq3 = lm(rgdpg ~ econg + co2g + eximg + factor(country), data=wo_china)
eq3_disp = tidy(eq3)
cat('\nEq3. Fixed Effect Model without China'); kable(eq3_disp, 'simple', digits=3); cat('\n')

eq4 = lm(rgdpg ~ econg + co2g + eximg + factor(country), data=country)
eq4_disp = tidy(eq4)
cat('\nEq4. Fixed Effect Model with China'); kable(eq4_disp, 'simple', digits=3); cat('\n')

eq5 = lm(rgdpg ~ econg + co2g + eximg + factor(year), data=wo_china)
eq5_disp = tidy(eq5)
cat('\nEq5. Year Fixed Effect Model without China'); kable(eq5_disp, 'simple', digits=3); cat('\n')

eq6 = lm(rgdpg ~ econg + co2g + eximg + factor(year), data=country)
eq6_disp = tidy(eq6)
cat('\nEq6. Year Fixed Effect Model with China'); kable(eq6_disp, 'simple', digits=3); cat('\n')

eq7 = lm(rgdpg ~ econg + co2g + eximg + factor(country) + factor(year), data=wo_china)
eq7_disp = tidy(eq7)
cat('\nEq7. Country and Year Fixed Effect Model without China'); kable(eq7_disp, 'simple', digits=3); cat('\n')

eq8 = lm(rgdpg ~ econg + co2g + eximg + factor(country) + factor(year), data=country)
eq8_disp = tidy(eq8)
cat('\nEq8. Country and Year Fixed Effect Model with China'); kable(eq8_disp, 'simple', digits=3); cat('\n')



### TREND MODELS ###

eq9 = lm(rgdpg ~ econg + co2g + year, data=wo_china)
eq9_disp = tidy(eq9)
cat('\nEq9. Time Trend Model without China'); kable(eq9_disp, 'simple', digits=3); cat('\n')

eq10 = lm(rgdpg ~ econg + co2g + year, data=country)
eq10_disp = tidy(eq10)
cat('\nEq10. Time Trend Model with China'); kable(eq10_disp, 'simple', digits=3); cat('\n')

# code to export all tables to text file
sink('tables.txt')
cat('\nEq1. Benchmark Equation: Pool OLS Model without China'); kable(eq1_disp, 'simple', digits=3); cat('\n')
cat('\nEq2. Pool OLS Model with China'); kable(eq2_disp, 'simple', digits=3); cat('\n')
cat('\nEq4. Fixed Effect Model with China'); kable(eq4_disp, 'simple', digits=3); cat('\n')
cat('\nEq5. Year Fixed Effect Model without China'); kable(eq5_disp, 'simple', digits=3); cat('\n')
cat('\nEq6. Year Fixed Effect Model with China'); kable(eq6_disp, 'simple', digits=3); cat('\n')
cat('\nEq7. Country and Year Fixed Effect Model without China'); kable(eq7_disp, 'simple', digits=3); cat('\n')
cat('\nEq8. Country and Year Fixed Effect Model with China'); kable(eq8_disp, 'simple', digits=3); cat('\n')
cat('\nEq9. Time Trend Model without China'); kable(eq9_disp, 'simple', digits=3); cat('\n')
cat('\nEq10. Time Trend Model with China'); kable(eq10_disp, 'simple', digits=3); cat('\n')
sink()



### PLOTTING TRENDS ###

# plotting function
plot_gdp_trends = function(country_data, country_name, country_title) {
  # country_data = data frame with country data
  # country_name = name of country as specified in country_data
  # country_title = country name as to appear in the title of the plot
  
  # create year vector for data frame initialization for all countries
  year = unique(country_data$year)
  # data on which to make predictions
  current = country_data[country_data$country == country_name,]
  # initialize data frame
  fit_yr = data.frame(year)
  # predict with OLS model without China
  fit_ols = data.frame(predict(eq1, current))
  # predict with fixed effect model with China
  fit_fe = data.frame(predict(eq4, current))
  # combine predictions in a single data frame
  fit = cbind(fit_yr, fit_ols, fit_fe)
  # rename columns
  colnames(fit) = c('year', 'yhat_ols', 'yhat_fe')
  # drop missing values
  fit = na.exclude(fit)
  
  ggplot() + 
    geom_line(data = fit, aes(x = year, y = yhat_ols, colour = 'OLS Model w/o China'), linetype="solid", size=1.25) +
    geom_point(data = fit, aes(x = year, y = yhat_ols, colour = 'OLS Model w/o China'), size=3) +
    
    geom_line(data = fit, aes(x = year, y = yhat_fe, colour = 'Fixed Effect Model w China'), linetype="solid", size=1.25) +
    geom_point(data = fit, aes(x = year, y = yhat_fe, colour = 'Fixed Effect Model w China'), size=3) +
    scale_colour_manual(name='', values=c('OLS Model w/o China'='blue', 'Fixed Effect Model w China'='chartreuse4')) + 
    theme(legend.position=c(0.2, 0.1), legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) + 
    xlab('') + ylab('(%)') + ggtitle(glue('{country_title} Real GDP Growth Rate and Model Estimation'))
}

plot_gdp_trends(country, 'United States', 'U.S.')
plot_gdp_trends(country, 'China', 'China')
plot_gdp_trends(country, 'Australia', 'Australia')
plot_gdp_trends(country, 'Brazil', 'Brazil')
plot_gdp_trends(country, 'Canada', 'Canada')
plot_gdp_trends(country, 'Germany', 'Germany')
plot_gdp_trends(country, 'India', 'India')
plot_gdp_trends(country, 'South Korea', 'South Korea')
plot_gdp_trends(country, 'United Kingdom', 'U.K.')
plot_gdp_trends(country, 'Russia', 'Russia')
plot_gdp_trends(country, 'Japan', 'Japan')
