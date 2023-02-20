library(quantmod)
library(TTR)

getSymbols("AAPL")

prices = AAPL$AAPL.Close

ma_fast = SMA(prices,n=50)
ma_slow = SMA(prices,n=200)

plot(ma_fast)
plot(ma_slow)

both = merge(prices,ma_fast,ma_slow)
plot(both)


data = data.frame(Price=prices,SMA50=ma_fast,SMA200=ma_slow)
names(data) = c("Prices","SMA50","SMA200")
View(data)
data$Signal = ifelse(data$SMA50 > data$SMA200,1,-1)
View(data$Signal)


data$Return = dailyReturn(prices)
data$Return
