rm(list = ls())
source("./helper_functions.R")
df = load_and_preprocess_data()

walkforward_df = generic_expanding_window("1984-12-31","1995-12-31",df,method = "tree")
tail(df)

plot(walkforward_df)
addLegend(legend.loc = "topleft",
          legend.names = c("Actual Returns","Predicted Returns"),
          col = c("red","black"))


View(merge(cumprod(1+walkforward_df$Actual_Returns),cumprod(1+walkforward_df$Predicted_Returns)))
plot(merge(cumprod(1+walkforward_df$Actual_Returns),cumprod(1+walkforward_df$Predicted_Returns)))



rolling_windowdf = generic_rolling_window("1984-12-31","1995-12-31",df,method_ = "tree")

View(merge(cumprod(1+rolling_windowdf$Actual_Returns),cumprod(1+rolling_windowdf$Predicted_Returns)))
plot(merge(cumprod(1+rolling_windowdf$Actual_Returns),cumprod(1+rolling_windowdf$Predicted_Returns)),main="Compare returns")
