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



varsetup_ts = merge(df$cfnaima3_ts,df$diffusion_ts,logrets_ts)["1984-12-31::1995-12-31"]
names(varsetup_ts) = c("cfnaima3_ts","diffusion_ts","logrets_ts")

var.lag.order = VARselect(y = varsetup_ts,lag.max = 10)$selection[[1]]
varmodel = VAR(varsetup_ts,p = var.lag.order)
varmodel

plot(df$returns_ts)

predict.training.vals = predict(varmodel,df["1984-12-31::2022"])
View(predict.training.vals[[1]]$diffusion_ts)
View(df$diffusion_ts)

predicted_next100 = predict(varmodel,n.ahead = 100)[[1]]$logrets_ts
length(logrets_ts["1995-12-31::1996-01-31"])
as.Date("1995-12-31")+100

which(index(df) == "1995-12-31")
index(df)[[234]]
