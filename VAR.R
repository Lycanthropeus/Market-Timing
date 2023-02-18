suppressMessages(suppressWarnings({
library(dplyr)
library(vars)
library(lubridate)
}))


cfnai_u = read.csv("./data/cfnai.csv")
cfnai_u$Date = as.Date(parse_date_time(cfnai_u$Date,"dmy"))
tibble(cfnai_u)

