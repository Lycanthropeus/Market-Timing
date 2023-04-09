#Read data and setup env
rm(list = ls())
source("./helper_functions.R")
df = load_and_preprocess_data()
logprices_ts = log(df$prices_ts)
logrets_ts = diff(logprices_ts)

# Checking stationarity of SP500 values
pp.test(df$prices_ts)$p.value   ##PP Test has null hypothesis of nonstationarity
kpss.test(df$prices_ts)$p.value ##KPSS Test has null hypothesis of stationarity
adf.test(df$prices_ts)$p.value ##ADF  Test has null hypothesis of nonstationarity

# Checking with log returns of SP500
pp.test(logrets_ts["1984-12-31::2022"])
kpss.test(logrets_ts["1984-12-31::2022"])
adf.test(logrets_ts["1984-12-31::2022"])

# Checking with CFNAI_MA3 ... 
pp.test(df$cfnaima3_ts)
kpss.test(df$cfnaima3_ts)
adf.test(df$cfnaima3_ts)

# ... and Diffusion
pp.test(df$diffusion_ts)
kpss.test(df$diffusion_ts)
adf.test(df$d)



varsetup_ts = merge(df$cfnaima3_ts,df$diffusion_ts,logrets_ts)["1984-12-31::2022"]
names(varsetup_ts) = c("cfnaima3_ts","diffusion_ts","logrets_ts")

var.lag.order = VARselect(y = varsetup_ts,lag.max = 10)$selection[[1]]
varmodel = VAR(varsetup_ts,p = 2)
summary(varmodel)

plot(df$returns_ts)

predict(varmodel)[[1]]$cfnaima3_ts
tail(df)

predict.training.vals = predict(varmodel,df["1984-12-31::2022"])
View(predict.training.vals[[1]]$diffusion_ts)
View(df$diffusion_ts)

predicted_next100 = predict(varmodel,n.ahead = 100)[[1]]$logrets_ts
# length(logrets_ts["1995-12-31::1996-01-31"])
# as.Date("1995-12-31")+100

which(index(df) == "1995-12-31")
index(df)[[234]]
varsetup_ts

logrets_predictions_expanding = varmodel_expanding_window("1984-12-31","1995-12-31",varsetup_ts)
compare_returns = merge(logrets_predictions_expanding$Predicted_Returns,df$returns_ts)
names(compare_returns) = c("Predicted_returns","Actual_returns")
x = compare_returns["1996::2022"]
plot(merge(cumprod(x$Predicted_returns),cumprod(1+x$Actual_returns)),main="Predicted Returns(black) vs Actual Returns(red)")

for(i in g){
  i = 10
  logrets_predictions_expanding = varmodel_expanding_window("1984-12-31","1995-12-31",varsetup_ts,i)
  compare_returns = merge(logrets_predictions_expanding$Predicted_Returns,df$returns_ts)
  names(compare_returns) = c("Predicted_returns","Actual_returns")
  x = compare_returns["1996::2022"]
  png(paste0("../../Images/expanding_window_lag",i,".png"))
  plot(merge(cumprod(x$Predicted_returns),cumprod(1+x$Actual_returns)),main=paste("Predicted Returns(black) vs Actual Returns(red) Lag Order:",i))
  dev.off()
}

logrets_predictions_rolling = varmodel_rolling_window("1984-12-31","1995-12-31",varsetup_ts)
compare_returns_rolling = merge(logrets_predictions_rolling$Predicted_Returns,df$returns_ts)
View(compare_returns_rolling)
names(compare_returns_rolling) = c("Predicted_returns","Actual_returns")
x = compare_returns_rolling["1996-02-28::2022"]
plot(merge(cumprod(x$Predicted_returns),cumprod(1+x$Actual_returns)),main="Predicted Returns(black) vs Actual Returns(red)")

for(i in g){
  i = 8
  logrets_predictions_rolling = varmodel_rolling_window("1984-12-31","1995-12-31",varsetup_ts)
  compare_returns_rolling = merge(logrets_predictions_rolling$Predicted_Returns,df$returns_ts)
  View(compare_returns_rolling)
  names(compare_returns_rolling) = c("Predicted_returns","Actual_returns")
  x = compare_returns_rolling["1996-02-28::2022"]
  png(paste0("../../Images/rolling_window_lag",i,".png"))
  plot(merge(cumprod(x$Predicted_returns),cumprod(1+x$Actual_returns)),main="Predicted Returns(black) vs Actual Returns(red)")
  dev.off()
}


# plot(merge(df$cfnaima3_ts,df$diffusion_ts,df$))
plot(varsetup_ts)

x = varsetup_ts$cfnaima3_ts
x
y = varsetup_ts$prices_ts
plot(x,y)
g = data.frame(x,y)
plot(g$cfnaima3_ts,g$prices_ts)
x = lag(varsetup_ts$cfnaima3_ts)
y = varsetup_ts$prices_ts
g__ = data.frame(x["1985::2022"],y["1985::2022"])
plot(g__$cfnaima3_ts,g__$prices_ts)


vxvcls = fredr(
  series_id = 'VXVCLS',
  observation_start = as.Date('1985-01-01'),
  observation_end = as.Date('2022-12-31'),
  frequency = 'm'
)
usslind = fredr(
  series_id = 'USSLIND',
  observation_start = as.Date('1985-01-01'),
  observation_end = as.Date('2022-12-31'),
  frequency = 'm'
) 
vxvcls_vs_sp500 = merge(vxvcls,sp500,by.x = "date",by.y = "Date")  
usslind_vs_sp500 = merge(usslind,sp500,by.x = "date",by.y = "Date")  
vxvcls_vs_sp500 = vxvcls_vs_sp500[,c(1,3,6,7)]
usslind_vs_sp500 = usslind_vs_sp500[,c(1,3,6,7)]

all_preds = merge(vxvcls_vs_sp500,usslind_vs_sp500,by="date")
all_preds = all_preds[,c(1,2,5,6,7)]
all_preds$value.x.discretized = ntile(all_preds$value.x,4)
all_preds$value.y.discretized = ntile(all_preds$value.y,4)

all_preds
tree_model = tree(returns.y ~ value.x.discretized + value.y.discretized,all_preds[1:92,])
train_pred = predict(tree_model)

plot(cumprod(1+all_preds$returns.y),type='l')
plot(cumprod(1+train_pred),type='l')


all_preds$new_predicts = train_pred

highchart(type = 'stock') %>% 
  hc_add_series(cumprod(1+all_preds$returns.y)) %>% 
  hc_add_series(cumprod(1+all_preds$new_predicts))
