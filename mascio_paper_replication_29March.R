library(dplyr)
ads = read.xlsx("./data/ads_gz.xlsx",sheet=1)
ads$Date = convertToDate(ads$Date)
gz = read.xlsx("./data/ads_gz.xlsx",sheet=2)
gz$Date = convertToDate(gz$Date)
gz = gz[,c(1,2,3)]

merge(ads,sp500,by="Date")
dplyr::inner_join()
gz

df_processed = ads %>% 
  inner_join(gz,by = "Date")   %>% 
  inner_join(sp500,by="Date") %>% 
  dplyr::select(c(1,2,3,6))

df_fin = df_processed[df_processed$Date < as.Date("2014-04-30"),]
df_fin


df_fin$up_down = ifelse(df_fin$returns > 0,1,0)
df_fin$lag_up_down = lag(df_fin$up_down)
View(df_fin)


df_fin = df_fin[2:length(df_fin$Date),]
df_fin.xts = as.xts(df_fin)
dates = index(df_fin.xts)

df_fin

length(df_fin$Date)


pred_val_list = c()

for(i in 1:198){
  training_data = df_fin[i:(i+24),]
  model = glm(lag_up_down~ADS_Index,training_data,family="binomial")
  predicted_val = predict(model,df_fin[i+25,],type="response")
  pred_val_list = c(pred_val_list,ifelse(predicted_val>0.5,1,0))
}
pred_val_list
length(df_fin[26:223,]$Date)

results = data.frame("Actual_Direction" = df_fin[26:223,]$lag_up_down,"Predicted_Direction" = pred_val_list)
cor(results$Actual_Direction,results$Predicted_Direction,method = "kendall")


sum(results$Actual_Direction == results$Predicted_Direction)/length(results$Actual_Direction)

pred_val_list_2 = c()
for(i in 1:198){
  training_data = df_fin[i:(i+24),]
  model = glm(lag_up_down~ADS_Index + GZ_Credit_Spread,training_data,family="binomial")
  predicted_val = predict(model,df_fin[i+25,],type="response")
  pred_val_list_2 = c(pred_val_list_2,ifelse(predicted_val>0.5,1,0))
}

results_2 = data.frame("Actual_Direction" = df_fin[26:223,]$lag_up_down,"Predicted_Direction" = pred_val_list_2)
cor(results_2$Actual_Direction,results_2$Predicted_Direction,method = "spearman")

sum(results_2$Actual_Direction == results_2$Predicted_Direction)/length(results_2$Actual_Direction)
View(results_2)

results_2$market_returns = lead(df_fin[26:223,]$returns)
results_2  = na.omit(results_2)
results_2$wealth_from_predicting = results_2$market_returns * results_2$Predicted_Direction
results_2$wealth_otherwise = results_2$market_returns * results_2$Actual_Direction


plot(cumprod(1+results_2$wealth_from_predicting),type='l',col='red')
lines(cumprod(1+results_2$wealth_otherwise),type='l',col='blue')
legend(c("Red","Blue"))
