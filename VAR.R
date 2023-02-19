suppressMessages(suppressWarnings({
library(dplyr)
library(vars)
library(lubridate)
library(ggplot2)
}))


cfnai_u = read.csv("./data/cfnai.csv")
cfnai_u$Date = as.Date(parse_date_time(cfnai_u$Date,"dmy"))
cfnai_u = tibble(cfnai_u);cfnai_u

ggplot(data = cfnai_u,mapping=aes(x=Date,y=SP500.Values)) +
  geom_point()+geom_line()

