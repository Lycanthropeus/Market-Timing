source("./helper_functions.R")
cfnai = load_and_preprocess_data()


library(tree)

# Todo @ Niranjan 
# First implement expanding window decision tree.

prices = cfnai$SP500.Values


prices_ts = as.xts(prices,order.by = cfnai$Date)
cfnai_ts = as.xts(cfnai$CFNAI,order.by = cfnai$Date)
cfnaima3_ts = as.xts(cfnai$CFNAI_MA3,order.by = cfnai$Date)
diffusion_ts = as.xts(cfnai$DIFFUSION,order.by = cfnai$Date)
df = merge(cfnai_ts,cfnaima3_ts,diffusion_ts,prices_ts)
df$returns_ts = Return.calculate(df$prices_ts,"discrete")

View(df)


training_set = df["1984-12-31::1995"] 

model = tree(returns_ts ~ cfnaima3_ts + diffusion_ts,data=training_set)
plot(model)
text(model,pretty=1)

predicted_init = predict(model,df["1996-01-31"])
df["1996-01-31"]

predicted_init$
# training_set_expand = df["1984-12-31::1996-31-01"]
# View(df$prices_ts)
# plot(df$prices_ts)


data.list <- split(df,'days')

### Expanding window ###
walkforward_df = data.frame(Date=as.Date("2029-09-01"),Predicted_Returns=3.5,Actual_Returns=5.5)
walkforward_df
lis = index(df)
for(idx in seq_along(lis)){
  x = as.Date(lis[[idx]])
  if(x >= "1995-12-31" & x<"2022-12-31"){
    training_data = df[paste0("1984-12-31::",x)]
    model = tree(returns_ts ~ diffusion_ts + cfnaima3_ts,data = training_data)
    pred_returns = predict(model,df[lis[[idx+1]]])
    actual_returns = df[lis[[idx+1]]]$returns_ts   
    loc_df = data.frame(x,as.numeric(actual_returns),as.numeric(pred_returns))
    names(loc_df) = c("Date","Actual_Returns","Predicted_Returns")
    walkforward_df = rbind(walkforward_df,loc_df)
  }
}
g = as.xts(cbind(walkforward_df$Actual_Returns,walkforward_df$Predicted_Returns),order.by = walkforward_df$Date)
names(g) = c("Actual_Returns","Predicted_Returns")
g = g["1995::2022"]
addLegend("topleft", on=1, 
          legend.names = c("Actual Returns", "Predicted Returns"), 
          lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "red"))

View(g)
View(merge(cumprod(1+g$Actual_Returns),cumprod(1+g$Predicted_Returns)))
plot(merge(cumprod(1+g$Actual_Returns),cumprod(1+g$Predicted_Returns)))


### Rolling window ###
rollingwindow_df = data.frame(Date=as.Date("2029-09-01"),Predicted_Returns=3.5,Actual_Returns=5.5)
rollingwindow_df

lis = index(df)
for(idx in seq_along(lis)){
  
  if(idx + 133 < length(lis)){
    sdate = as.Date(lis[[idx]])
    edate = as.Date(lis[[idx+133]])
    
    training_data_rolling = df[paste0(sdate,"::",edate)]
    model_rolling = tree(returns_ts ~ diffusion_ts + cfnaima3_ts,data = training_data_rolling)
    
    next_date = lis[[idx+134]]
    pred_returns = predict(model_rolling,df[next_date])
    actual_returns = df[next_date]$returns_ts
    
    # print(paste(next_date,pred_returns,actual_returns,sep="..."))
    
    loc_df = data.frame(next_date,as.numeric(actual_returns),as.numeric(pred_returns))
    names(loc_df) = c("Date","Actual_Returns","Predicted_Returns")
    
    rollingwindow_df = rbind(rollingwindow_df,loc_df)
  }
}  
h = as.xts(cbind(rollingwindow_df$Actual_Returns,rollingwindow_df$Predicted_Returns),order.by = rollingwindow_df$Date)
names(h) = c("Actual_Returns","Predicted_Returns")
h = h["1995::2022"]
View(h)  

View(merge(cumprod(1+h$Actual_Returns),cumprod(1+h$Predicted_Returns)))
plot(merge(cumprod(1+h$Actual_Returns),cumprod(1+h$Predicted_Returns)),main = "Comparison of returns")

addLegend("topleft", on=1, 
          legend.names = c("Actual Returns", "Predicted Returns"), 
          lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "red"))
