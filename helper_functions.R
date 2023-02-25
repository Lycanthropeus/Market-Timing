load_and_preprocess_data = function() {
  setwd("D:/4th Sem Work/Research Work_Rahul/Market-Timing/")
  .libPaths("D:/4th Sem Work/Research Work_Rahul/Market-Timing/libs/")
  suppressMessages(suppressWarnings({
    library(dplyr)
    library(vars)
    library(lubridate)
    library(ggplot2)
    library(tseries)
    library(xts)
    library(PerformanceAnalytics)
  }))
  cfnai_u = read.csv("./data/cfnai.csv")
  cfnai_u$Date = as.Date(parse_date_time(cfnai_u$Date,"dmy"))
  cfnai_u = tibble(cfnai_u);cfnai_u
  return (cfnai_u)
}

expanding_window_error = function(sdate){
  
}