##Use ADS. GZ, and EPU
source("./helper_functions.R")
usethis::edit_r_environ()
load_and_preprocess_data()

getwd()
ads = read.xlsx("./data/ads_gz.xlsx",sheet = 1)
ads$Date = convertToDate(ads$Date)

gz = read.xlsx("./data/ads_gz.xlsx",sheet = 2)
gz = gz[,c(1,2,3)]
gz$Date = convertToDate(gz$Date)

sp500 = read.csv("./data/sp500.csv")
sp500$Date = as.Date(sp500$Date,format = "%d-%m-%Y")
sp500.prices = as.xts(sp500$Adj.Close,order.by = as.Date(sp500$Date,format = "%d-%m-%Y"))

# ads_gz = merge(ads,gz,by = c("Date"))
# ads_gz_sp500 = merge(ads_gz,sp500,by="Date",)
sp500.prices
ads.ts = as.xts(ads)
gz.ts = as.xts(gz)

ads_gz.ts = merge.xts(ads.ts,gz.ts,join = "outer")
fina_ts = merge.xts(ads_gz.ts,sp500.prices,join="outer")
fina_ts = na.omit(fina_ts)["1985::2022"]
plot(fina_ts$sp500.prices)

length(df2["1985::2014"])
# monthly_data = df2[endpoints(x = df2,on="months",k=1),]
# 
# length(monthly_data)

data_series = fredr(
  series_id = "VIXCLS",
  observation_start = as.Date("1985-01-01"),
  observation_end = as.Date("2022-12-31"),
  frequency = 'w'
)
sp500 = read.csv("./data/sp500.csv")
sp500$Date = as.Date(sp500$Date,format="%d-%m-%Y")
sp500 = sp500[,c(1,6)]
sp500$sp500.returns = Return.calculate(sp500)
sp500$returns = sp500$sp500.returns$x
sp500 = sp500[,c(1,2,4)]
str(sp500)
sp500
 
# vixcls_vs_sp500 = merge(data_series,sp500,by.x = "date",by.y = "Date")
# vixcls_vs_sp500 = vixcls_vs_sp500[,c(1,3,6,7)]
# names(vixcls_vs_sp500) = c("Date","Value","SP500","SP500.Returns")
# vixcls.ts = na.omit(as.xts(vixcls_vs_sp500))
# 
# vixcls.ts$ranked = ntile(vixcls_vs_sp500$Value,4)
# vixcls_vs_sp500$rank = vixcls.ts$ranked
# vixcls_vs_sp500
# 
# avg_rets_plot = vixcls_vs_sp500 %>% group_by(rank) %>% summarise(avg_return = mean(SP500.Returns))
# 
# barplot(avg_rets_plot$avg_return,xlab = "Rank",ylab="Avg Returns",names.arg=c(1,2,3,4),space=5,
#         border="red",col="blue",density=10)
# 
# plot(vixcls_vs_sp500$Value,vixcls_vs_sp500$SP500.Returns)
# 
# vixcls_vs_sp500$lag_rets = lag(vixcls_vs_sp500$SP500.Returns)
# 
# 
# avg_rets_lag_plot = na.omit(vixcls_vs_sp500) %>% group_by(rank) %>% summarise(avg_return = mean(lag_rets))
# avg_rets_lag_plot
# 
# barplot(avg_rets_lag_plot$avg_return,xlab = "Rank",ylab="Avg Returns",names.arg=c(1,2,3,4),space=5,
#         border="red",col="blue",density=10,main="VIXCLS")


list_of_predictors = read.xlsx("./data/predictor_list.xlsx",sheet = 4)
tibble(list_of_predictors)

visualize_predictors = function(predictor,sp500){

  tryCatch(
    expr = {
      data_series = fredr(
              series_id = predictor,
              observation_start = as.Date("1985-01-01"),
              observation_end = as.Date("2022-12-31"),
              frequency = 'w'
            )
      print(paste("Weekly data fetch successful!",predictor))
    },
    error = function(e) {
      data_series = fredr(
        series_id = predictor,
        observation_start = as.Date("1985-01-01"),
        observation_end = as.Date("2022-12-31"),
        frequency = 'm'
      )
      print(paste("Weekly data unavailable, fetching monthly...",predictor))
      unavailable_flag = TRUE
    }
  )
  
  # if(unavailable_flag){
  #   data_series = fredr (
  #             series_id = predictor,
  #             observation_start = as.Date("1985-01-01"),
  #             observation_end = as.Date("2022-12-31"),
  #             frequency = 'm'
  #           )
  #   predictor_vs_sp500 = merge(data_series,sp500,by.x = "date",by.y = "Date")
  # }
  # unavailable_flag = FALSE
  predictor_vs_sp500 = merge(data_series,sp500,by.x = "date",by.y = "Date")  
  predictor_vs_sp500 = predictor_vs_sp500[,c(1,3,6,7)]
  names(predictor_vs_sp500) = c("Date","Value","SP500","SP500.Returns")
  
  predictor_vs_sp500 = na.omit(predictor_vs_sp500)
  predictor_vs_sp500$rank = ntile(predictor_vs_sp500$Value,4)
  
  folder_path = paste0("./output_4_new_iter/",predictor)
  dir.create(folder_path)
  
  avg_rets_plot = predictor_vs_sp500 %>% 
                    group_by(rank) %>% 
                      summarise(avg_return = mean(SP500.Returns))

  png(paste0(folder_path,"/avg_returns_across_quartiles.png"))
  barplot(avg_rets_plot$avg_return,xlab = "Rank",ylab="Avg Returns",
              names.arg=c(1,2,3,4),space=5,
              border="red",col="blue",density=10,main=predictor)
  dev.off()
  
  predictor_vs_sp500$lag_rets = lag(predictor_vs_sp500$SP500.Returns)
  avg_rets_lag_plot = na.omit(predictor_vs_sp500) %>% 
                        group_by(rank) %>% 
                        summarise(avg_return = mean(lag_rets))
  
  png(paste0(folder_path,"/avg_returns_across_quartiles_lag1.png"))
  barplot(avg_rets_lag_plot$avg_return,xlab = "Rank",ylab="Avg Returns",
              names.arg=c(1,2,3,4),space=5,
              border="red",col="blue",density=10,main=predictor)
  dev.off()
  
  png(paste0(folder_path,"/scatterplot_nolag.png"))
  plot(predictor_vs_sp500$Value,predictor_vs_sp500$SP500.Returns,
      xlab = predictor,ylab="Returns")
  dev.off()
  
  png(paste0(folder_path,"/scatterplot_lag1.png"))
  plot(predictor_vs_sp500$Value,predictor_vs_sp500$lag_rets,
       xlab = predictor,ylab="Returns")
  dev.off()
  
  write.csv(x=predictor_vs_sp500,file = paste0(folder_path,"/",predictor,".csv"))
}

# gg = list.files("./output_3_experimental/")
list_of_predictors$Predictor
for (ele in list_of_predictors$Predictor){
  # if(any(gg == ele)){
  #   print(paste(ele,"already present. Skipping..."))
  #   next
  # }
  visualize_predictors(ele,sp500)
}


visualize_predictors_2 = function(predictor,sp500,sampled_at){
  data_series = fredr(
    series_id = predictor,
    observation_start = as.Date("1985-01-01"),
    observation_end = as.Date("2022-12-31"),
    frequency = sampled_at
  )
  # data_series = read.xlsx("C:/Users/Niru/Downloads/External Data Sources (Non-FREDR)/US_Policy_Uncertainty_Data.xlsx")
  # predictor_vs_sp500 = merge(data_series,sp500,by.x = "Date",by.y = "Date")
  # data_series$Date = as.Date(data_series$Date)
  # predictor_vs_sp500 = predictor_vs_sp500[,c(1,2,4)]
  # names(predictor_vs_sp500) = c("Date","Value","SP500.Returns")
  
  predictor_vs_sp500 = merge(data_series,sp500,by.x = "date",by.y = "Date")
  predictor_vs_sp500 = predictor_vs_sp500[,c(1,3,6,7)]
  names(predictor_vs_sp500) = c("Date","Value","SP500","SP500.Returns")
  
  predictor_vs_sp500 = na.omit(predictor_vs_sp500)
  predictor_vs_sp500$rank = ntile(predictor_vs_sp500$Value,4)
  
  folder_path = paste0("./output_cm_4thApril_mod/",predictor)
  dir.create(folder_path)
  
  avg_rets_plot = predictor_vs_sp500 %>% 
    group_by(rank) %>% 
    summarise(avg_return = mean(SP500.Returns))
  
  png(paste0(folder_path,"/avg_returns_across_quartiles.png"))
  barplot(avg_rets_plot$avg_return,xlab = "Rank",ylab="Avg Returns",
          names.arg=c(1,2,3,4),space=5,
          border="red",col="blue",density=10,main=predictor)
  dev.off()
  
  predictor_vs_sp500$lag_preds = lag(predictor_vs_sp500$Value)
  avg_rets_lag_plot = na.omit(predictor_vs_sp500) %>% 
    group_by(rank) %>% 
    summarise(avg_return = mean(SP500.Returns))
  
  png(paste0(folder_path,"/avg_returns_across_quartiles_lag1.png"))
  barplot(avg_rets_lag_plot$avg_return,xlab = "Rank",ylab="Avg Returns",
          names.arg=c(1,2,3,4),space=5,
          border="red",col="blue",density=10,main=predictor)
  dev.off()
  
  png(paste0(folder_path,"/scatterplot_nolag.png"))
  plot(predictor_vs_sp500$Value,predictor_vs_sp500$SP500.Returns,
       xlab = predictor,ylab="Returns")
  dev.off()
  
  png(paste0(folder_path,"/scatterplot_lag1.png"))
  plot(predictor_vs_sp500$lag_preds,predictor_vs_sp500$SP500.Returns,
       xlab = predictor,ylab="Returns")
  dev.off()
  
  write.csv(x=predictor_vs_sp500,file = paste0(folder_path,"/",predictor,".csv"))
}

for(i in 1:length(list_of_predictors$Predictor)){
  predictor = list_of_predictors[i,]$Predictor
  sampled_at = list_of_predictors[i,]$Frequency
  visualize_predictors_2(predictor,sp500,sampled_at = sampled_at)
}


vixcls = read.csv("D:/4th Sem Work/Research Work_Rahul/Market-Timing/output_cm_28thMarch_final/EVZCLS/EVZCLS.csv")
avg_rets_plot = vixcls %>% 
  group_by(rank) %>% 
  summarise(avg_return = mean(SP500.Returns))

avg_rets_lag_plot = na.omit(vixcls) %>% 
  group_by(rank) %>% 
  summarise(avg_return = mean(lag_rets))
avg_rets_lag_plot
plot(avg_rets_lag_plot)
plot(avg_rets_plot)
boxplot(vixcls$SP500.Returns ~ vixcls$rank)
plot(y=vixcls$SP500.Returns,x=vixcls$rank)
vixcls$SP500.Returns.Discretized = ntile(vixcls$SP500.Returns,4)

chisq.test(vixcls$SP500.Returns.Discretized,vixcls$rank)

chisq.test(lag(vixcls$SP500.Returns.Discretized),vixcls$rank)$p.val


master_path = "D:/4th Sem Work/Research Work_Rahul/Market-Timing/output_cm_28thMarch_whittled_down/"
predictor_list = list.files("D:/4th Sem Work/Research Work_Rahul/Market-Timing/output_cm_28thMarch_whittled_down/")
for(filename in predictor_list){
  filepath = paste0(master_path,"/",filename,"/",filename,".csv")
  pred = read.csv(filepath)
  pred$SP500.Returns.Discretized = ntile(pred$SP500.Returns,4)
  if(chisq.test(lag(pred$SP500.Returns.Discretized),pred$rank)$p.val<0.05){
    print(paste("Selected...",filename))
  }
}

selected_predictor_list = list.files("D:/4th Sem Work/Research Work_Rahul/Market-Timing/output_cm_28thMarch_selected_predictors")
for(filename in predictor_list){
  filepath = paste0(master_path,"/",filename,"/",filename,".csv")
  pred = read.csv(filepath)

  png(paste0(master_path,"/",filename,"/",filename,"boxplot.png"))
  boxplot(lag(pred$SP500.Returns) ~ pred$rank,col='blue',ylab="returns",xlab="rank",main=filename,pch=21,bg='lightblue')
  dev.off()
    
  avg_rets_lag_plot = na.omit(pred) %>% 
    group_by(rank) %>% 
    summarise(avg_return = mean(lag_rets))

  png(paste0(master_path,"/",filename,"/",filename,"avg_returns_lag_plot.png"))
  plot(avg_rets_lag_plot,pch=21,bg='lightblue',col='blue',type='b',cex=2,xaxp=c(1,4,3),main=filename)
  dev.off()
}
