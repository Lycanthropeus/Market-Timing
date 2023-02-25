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
  cfnai = read.csv("./data/cfnai.csv")
  cfnai$Date = as.Date(parse_date_time(cfnai$Date,"dmy"))
  cfnai = tibble(cfnai);
  prices_ts = as.xts(cfnai$SP500.Values,order.by = cfnai$Date)
  cfnai_ts = as.xts(cfnai$CFNAI,order.by = cfnai$Date)
  cfnaima3_ts = as.xts(cfnai$CFNAI_MA3,order.by = cfnai$Date)
  diffusion_ts = as.xts(cfnai$DIFFUSION,order.by = cfnai$Date)
  df = merge(cfnai_ts,cfnaima3_ts,diffusion_ts,prices_ts)
  df$returns_ts = Return.calculate(df$prices_ts,"discrete")
  return (df)
}

expanding_window_tree = function(sdate,edate){
  walkforward_df = data.frame(Date=as.Date("2029-09-01"),Predicted_Returns=3.5,Actual_Returns=5.5)
  lis = index(df)
  for(idx in seq_along(lis)){
    x = as.Date(lis[[idx]])
    if(x >= sdate & x < edate){
      training_data = df[paste0("1984-01-31::",x)]
      model = tree(returns_ts ~ diffusion_ts + cfnaima3_ts,data = training_data)
      loc_prediction = predict(model,df[lis[[idx+1]]])
      
      g = data.frame(x,as.numeric(df[lis[[idx+1]]]$returns_ts),as.numeric(predict(model,df[lis[[idx+1]]])))
      names(g) = c("Date","Predicted_Returns","Actual_Returns")
      
      walkforward_df = rbind(walkforward_df,g)
    }
  }
  retval = as.xts(cbind(walkforward_df$Predicted_Returns,walkforward_df$Actual_Returns),order.by = walkforward_df$Date)
  names(retval) = c("Predicted_Returns","Actual_Returns")
  return (retval)
}


test_module = function(x,y,method_){
  func_ = match.fun(method_)
  return (func_(x,y))
}


generic_expanding_window = function(sdate,edate,method_,df){
  func_ = match.fun(method_)
  dates = index(df)
  
  walkforward_df = data.frame(Date=as.Date("2029-09-01"),Predicted_Returns=3.5,Actual_Returns=5.5)
  
  for(idx in seq_along(dates)){
    date = dates[[idx]]
    if(date >= edate && date < "2022-12-31"){
      training_data = df[paste0(sdate,"::",date)]
      next_date = dates[[idx+1]]
      model = func_(returns_ts ~ diffusion_ts + cfnaima3_ts,data = training_data)
      loc_prediction = predict(model,df[next_date])
      g = data.frame(next_date,as.numeric(loc_prediction),as.numeric(df[next_date]$returns_ts))
      names(g) = c("Date","Predicted_Returns","Actual_Returns")
      
      walkforward_df = rbind(walkforward_df,g)
    }
  }
  return (walkforward_df)
}

generic_rolling_window = function(sdate,edate,method){
  func_ = match.fun(method_)
  
  
}
