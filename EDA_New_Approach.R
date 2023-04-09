rm(list = ls())
source("./helper_functions.R")
library(highcharter)
df = load_and_preprocess_data()
plot(df$cfnai_ts)

percent_ranks = as.xts(ntile(df$cfnai_ts,n=10),order.by = index(df$cfnai_ts))
plot(percent_ranks)
ntile(df$cfnai_ts,n=10)

df2 = data.frame(ntile(df$cfnai_ts,n=10),df$returns_ts)
df2 = df2[2:length(df2$ntile.df.cfnai_ts..n...10.),]
df2
# df2
gg = df2 %>% 
        group_by(ntile.df.cfnai_ts..n...10.) %>% 
            summarise(mean_returns_bucket = mean(returns_ts))

plot(gg$ntile.df.cfnai_ts..n...10.,gg$mean_returns_bucket)

  
highchart(type = "stock") %>% 
  hc_add_series(df$cfnai_ts,type="line",
                color = "green")

highchart(type = "stock") %>% 
  hc_add_series(df$prices_ts,type="line",
                color = "magenta")

dense_rank = dplyr::dense_rank(df$cfnai_ts )
data.frame(dense_rank,df$returns_ts)
sort(unique(dense_rank))

df4 = data.frame(ntile(cfnai$cfnai_ts,n = 4),df$returns_ts)
yy = df4 %>% filter(!is.na(df4$returns_ts)) %>%  
  group_by(ntile.cfnai.cfnai_ts..n...4.) %>% 
  summarise(avg_rets = mean(returns_ts)) 

qq = ntile(cfnai$cfnai_ts,10)
which(df$cfnai_ts == min(df$cfnai_ts))
qq[426]
plot(qq)

ads = as.xts(ads)
names(ads) = c("Date","ADS")
ads
highchart(type = "stock") %>% 
  hc_add_series(ads,type="line",
                color = "purple")
max(ads$Date)
min(ads$Date)
plot.xts(ads)



highchart(type = "stock") %>% 
  # hc_add_series(df$cfnai_ts,type="scatter",color = "purple",name="cfnai") %>% 
  # hc_add_series(df$returns_ts,type="scatter",color = "blue",name="returnsSP500") %>% 
  hc_add_series(ads,type="line",color = "purple",name="ads") %>% 
  hc_tooltip(pointFormat = '{point.x: %Y-%m-%d}')


hchart()

tedrate = read.xlsx("./data/new_data_consolidated.xlsx",sheet = 1)
tedrate$date = convertToDate(tedrate$date)
plot(x=tedrate$date,y=tedrate$value,type = 'l')
highchart(type = "stock") %>% 
  hc_add_series(tedrate$value,type="line",color = "purple",name="TED rate")

tedrate$date
