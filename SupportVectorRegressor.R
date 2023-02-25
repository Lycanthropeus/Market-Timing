rm(list = ls())
source("./helper_functions.R")
df = load_and_preprocess_data()
# 
# library(caret)
# 
# View(cfnai)
# 
# 
# 
# caret::train()
# 
# df
generic_expanding_window("1984-12-31","1995-12-31",df,method = "tree")
tail(df)
