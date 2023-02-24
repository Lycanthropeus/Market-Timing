suppressMessages(suppressWarnings({
library(dplyr)
library(vars)
library(lubridate)
library(ggplot2)
library(tseries)
library(xts)
library(PerformanceAnalytics)
}))

.libPaths("D:/4th Sem Work/Research Work_Rahul/Market-Timing/libs/")

#Read data
cfnai_u = read.csv("./data/cfnai.csv")
cfnai_u$Date = as.Date(parse_date_time(cfnai_u$Date,"dmy"))
cfnai_u = tibble(cfnai_u);cfnai_u

#Format as XTS object for pretty plotting
cfnai_pl = as.xts(cfnai_u$SP500.Values,order.by = cfnai_u$Date,frequency=12)


# Checking for stationarity
pp.test(cfnai_u$SP500.Values)        
kpss.test(cfnai_u$SP500.Values)
adf.test(cfnai_u$SP500.Values)

# Checking with log S&P500
log_sp500 = log(cfnai_u$SP500.Values)
pp.test(log_sp500)
kpss.test(log_sp500)
adf.test(log_sp500)

# Checking with diff of log S&P500
diff(log_sp500)
pp.test(diff(log_sp500))
kpss.test(diff(log_sp500))
adf.test(diff(log_sp500))


#Checking if CFNAI series is stationary

my_data_series = data.frame(diff(cfnai_u$CFNAI),diff(cfnai_u$SP500.Values))
my_data_series

var_model = vars::VAR(my_data_series,p=2,type = "const")
var_model
summary(var_model)
