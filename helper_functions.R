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