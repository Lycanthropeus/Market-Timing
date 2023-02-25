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
View(df)

df$returns_ts = Return.calculate(df$prices_ts,"discrete")

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

# walkforward_df = list("Date"=,"Predicted_Returns"=3,"Actual_Returns"=5)
# names(walkforward_df) = c("Date","Predicted_Returns","Actual_Returns")
walkforward_df = data.frame("Predicted_Returns"=3.5,"Actual_Returns"=5.5)
walkforward_df


lis = index(df)
for(idx in seq_along(lis)){
  x = as.Date(lis[[idx]])
  if(x >= "1995-12-31" & x<"2022-12-31"){
    training_data = df[paste0("1984-01-31::",x)]
    model = tree(returns_ts ~ diffusion_ts + cfnaima3_ts,data = training_data)
    loc_prediction = predict(model,df[lis[[idx+1]]])
    
    g = data.frame(as.numeric(df[lis[[idx+1]]]$returns_ts),as.numeric(predict(model,df[lis[[idx+1]]])))
    names(g) = c("Predicted_Returns","Actual_Returns")
    
    walkforward_df = rbind(walkforward_df,g)
  }
}
walkforward_df
# as.xts(walkforward_df,order.by = )
# i = 1
# for(date in names(data.list)){
#   if(i < 458) {
#     if(!date <= "1996-01-31"){
#       training_data = df[paste0("1984-12-31::",date)]
#       model = tree(returns_ts ~ cfnaima3_ts + diffusion_ts,data = training_data)
#       predicted_next = predict(model,df[i+1,])
#       
#       print(predicted_next)
#       # c = df[i+1,]$returns_ts
#       # next_date = as.Date(index(df[i+1,])) 
#       # loc_list = list("Date" = next_date,"Predicted_Returns" = predicted_next,"Actual_Returns" = c)
#       # # walkforward_df = append(walkforward_df, loc_list)
#       # print(loc_list)
#       # walkforward_df %>% add_row()
#       # walkforward_df = rbind.data.frame(walkforward_df,loc_list)
#     }
#     i = i+1
#   }
# }
