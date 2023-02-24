library(tree)
library(PerformanceAnalytics)

# Todo @ Niranjan 
# First implement expanding window decision tree.
source("./helper_functions.R")
cfnai = load_and_preprocess_data()

prices = cfnai$SP500.Values


prices_ts = as.xts(prices,order.by = cfnai$Date)
cfnai_ts = as.xts(cfnai$CFNAI,order.by = cfnai$Date)
cfnaima3_ts = as.xts(cfnai$CFNAI_MA3,order.by = cfnai$Date)
diffusion_ts = as.xts(cfnai$DIFFUSION,order.by = cfnai$Date)

df = merge(cfnai_ts,cfnaima3_ts,diffusion_ts,prices_ts)
View(df)

df$returns_ts = Return.calculate(df$prices_ts,"discrete")

training_set = df["1984-12-31::1995"] 

model = tree(returns_ts ~ cfnaima3_ts + diffusion_ts,data=df)
plot(model)
text(model,pretty=1)

predict(model,df["1996-01-31"])
df["1996-01-31"]

plot(df$returns_ts)
# training_set_expand = df["1984-12-31::1996-31-01"]
View(df$prices_ts)
plot(df$prices_ts)
