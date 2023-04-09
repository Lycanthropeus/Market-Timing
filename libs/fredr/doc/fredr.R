## ----setup, include = FALSE---------------------------------------------------
library(fredr)

knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 5,
  eval = fredr_has_key(),
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(fredr)

## ----fredr_set_key, eval=FALSE------------------------------------------------
#  fredr_set_key("abcdefghijklmnopqrstuvwxyz123456")

## ----fredr_series_observations, message=FALSE, warning=FALSE------------------
fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2000-01-01")
)

## ----fredr_series_observations2, message=FALSE, warning=FALSE-----------------
fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2000-01-01"),
  frequency = "q", # quarterly
  units = "chg" # change over previous value
)

## ----fredr_series_observations3, message=FALSE, warning=FALSE-----------------
library(dplyr)
library(ggplot2)

popular_funds_series <- fredr_series_search_text(
    search_text = "federal funds",
    order_by = "popularity",
    sort_order = "desc",
    limit = 1
)

popular_funds_series_id <- popular_funds_series$id

popular_funds_series_id %>%
  fredr(
    observation_start = as.Date("1990-01-01"),
    observation_end = as.Date("2000-01-01")
  ) %>%
  ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
    geom_line() +
    labs(x = "Observation Date", y = "Rate", color = "Series")

## ----fredr_series_observations4, message=FALSE, warning=FALSE-----------------
library(purrr)

map_dfr(c("UNRATE", "FEDFUNDS"), fredr) %>%
  ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
    geom_line() +
    labs(x = "Observation Date", y = "Rate", color = "Series")

## ----fredr_series_observations5, message=FALSE, warning=FALSE-----------------
params <- list(
  series_id = c("UNRATE", "OILPRICE"),
  frequency = c("m", "q")
)

pmap_dfr(
  .l = params,
  .f = ~ fredr(series_id = .x, frequency = .y)
)

## ----fredr_series_observations6, message=FALSE, warning=FALSE-----------------
library(xts)

gnpca <- fredr(series_id = "GNPCA", units = "log") %>%
  mutate(value = value - lag(value)) %>%
  filter(!is.na(value))

gnpca_xts <- xts(
  x = gnpca$value,
  order.by = gnpca$date
)

gnpca_xts %>%
  StructTS() %>%
  residuals() %>%
  acf(., main = "ACF for First Differenced real US GNP, log")

## ----fredr_endpoints1, width = 180--------------------------------------------
fredr_endpoints

## ----fredr_docs, message=FALSE, warning=FALSE, eval=FALSE---------------------
#  fredr_docs()

## ----fredr_request1-----------------------------------------------------------
fredr_request(
  endpoint = "tags/series", 
  tag_names = "population;south africa", 
  limit = 25L
)

## ----fredr_request2-----------------------------------------------------------
fredr_request(
  endpoint = "series/observations",
  series_id = "UNRATE",
  to_frame = FALSE
)

