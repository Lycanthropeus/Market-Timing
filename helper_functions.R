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
    library(tree)
    library(openxlsx)
    library(fredr)
    library(usethis)
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
  walkforward_df.ts = as.xts(cbind(walkforward_df$Predicted_Returns,walkforward_df$Actual_Returns),
                            order.by=walkforward_df$Date)
  names(walkforward_df.ts) = c("Predicted_Returns","Actual_Returns")
  return (walkforward_df.ts["::2022"])
}

generic_rolling_window = function(sdate,edate,method_,df){
  func_ = match.fun(method_)
  dates = index(df)
  
  rollingwindow_df = data.frame(Date=as.Date("2029-09-01"),Predicted_Returns=3.5,Actual_Returns=5.5)
  
  for(idx in seq_along(dates)){
    if(idx + 133 < length(dates)){
      
      sdate_ = as.Date(dates[[idx]])
      edate_ = as.Date(dates[[idx+133]])
      
      training_data_rolling = df[paste0(sdate_,"::",edate_)]
      
      if(method_ == "tree"){
        model_rolling = func_(returns_ts ~ diffusion_ts + cfnaima3_ts,data = training_data_rolling)
      }
      else{
        model_rolling = func_(returns_ts ~ diffusion_ts + cfnaima3_ts,data = training_data_rolling,method = "svmLinear")
      }
      next_date = dates[[idx+134]]
      pred_returns = predict(model_rolling,df[next_date])
      actual_returns = df[next_date]$returns_ts
      
      
      loc_df = data.frame(next_date,as.numeric(actual_returns),as.numeric(pred_returns))
      names(loc_df) = c("Date","Actual_Returns","Predicted_Returns")
      
      rollingwindow_df = rbind(rollingwindow_df,loc_df)
    }
  }
  rollingwindow_df.ts = as.xts(cbind(rollingwindow_df$Predicted_Returns,rollingwindow_df$Actual_Returns),
                             order.by=rollingwindow_df$Date)
  names(rollingwindow_df.ts) = c("Predicted_Returns","Actual_Returns")
  return (rollingwindow_df.ts["::2022"])
}

plot_ts = function(df_,title){
  plot(df_)
  addLegend(
    legend.loc = "topleft",
    legend.names = c("Predicted Returns","Actual Returns"),
    col = c("red","black")
  )
}

varmodel_expanding_window = function(sdate,edate,df_cfnai,technique="dynamic"){
  dates = index(df_cfnai)
  
  walkforward_df = data.frame(Date=as.Date("2029-09-01"),Predicted_Returns=3.5)
  
  for(idx in seq_along(dates)){
    date = dates[[idx]]
    # print(date)
    if(date >= edate && date < "2022-12-31"){
      training_data = df_cfnai[paste0(sdate,"::",date)]
      next_date = dates[[idx+1]]
      # model = func_(returns_ts ~ diffusion_ts + cfnaima3_ts,data = training_data)
      if(is.numeric(technique)){
        model.lag.order = technique
      }
      else{
        if(technique == "dynamic")
          model.lag.order = VARselect(training_data,lag.max = 10)$selection[[1]]  
      }
      model = VAR(training_data,p = model.lag.order)
      
      loc_prediction = predict(model,n.ahead=1)[[1]]$logrets_ts[[1]]
      g = data.frame(next_date,exp(as.numeric(loc_prediction)))
      names(g) = c("Date","Predicted_Returns")
      # 
      walkforward_df = rbind(walkforward_df,g)
    }
  }
  walkforward_df.ts = as.xts(walkforward_df$Predicted_Returns,
                             order.by=walkforward_df$Date)
  names(walkforward_df.ts) = c("Predicted_Returns")
  return (walkforward_df.ts["::2022"])
}


varmodel_rolling_window = function(sdate,edate,df_cfnai,technique="dynamic"){
  dates = index(df_cfnai)
  rollingwindow_df = data.frame(Date=as.Date("2029-09-01"),Predicted_Returns=3.5)
  
  for(idx in seq_along(dates)){
    if(idx + 133 < length(dates)){
      
      sdate_ = as.Date(dates[[idx]])
      edate_ = as.Date(dates[[idx+133]])
      
      training_data_rolling = df_cfnai[paste0(sdate_,"::",edate_)]
      next_date = dates[[idx+134]]
      
      if(is.numeric(technique)){
        model.lag.order = technique
      }
      else{
        if(technique == "dynamic")
          model.lag.order = VARselect(training_data_rolling,lag.max = 10)$selection[[1]]  
      }
      
      model_rolling = VAR(training_data_rolling,p = model.lag.order)
      pred_returns= predict(model_rolling,n.ahead=1)[[1]]$logrets_ts[[1]]
      loc_df = data.frame(next_date,exp(as.numeric(pred_returns)))
      names(loc_df) = c("Date","Predicted_Returns")
      
      rollingwindow_df = rbind(rollingwindow_df,loc_df)
    }
  }
  
  rollingwindow_df.ts = as.xts(rollingwindow_df$Predicted_Returns,
                             order.by=rollingwindow_df$Date)
  names(rollingwindow_df.ts) = c("Predicted_Returns")
  return (rollingwindow_df.ts["::2022"])
}

load_predictors = function(){
  wb = createWorkbook()
  list_of_predictors = tibble(read.xlsx("./data/consolidated_data.xlsx"))
  for(idx in seq_along(list_of_predictors$Predictor)){
    pred = list_of_predictors$Predictor[[idx]]
    addWorksheet(wb,paste0("Sheet_",idx))
    data_series = fredr(
      series_id = pred,
      observation_start = as.Date("1985-01-01"),
      observation_end = as.Date("2022-12-31"),
      frequency = 'm'
    )
    writeData(wb, paste0("Sheet_",idx), data_series, startRow = 1, startCol = 1)
    print(paste(pred,"written successfully.."))
  }
  saveWorkbook(wb,"./data/new_data_consolidated.xlsx")
}
