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

## ----fredr_sources1-----------------------------------------------------------
fredr_sources(limit = 10)

## ----fredr_source1------------------------------------------------------------
fredr_source(source_id = 6L)

## ----fredr_source_releases1---------------------------------------------------
fredr_source_releases(
  source_id = 1L,
  limit = 10L
)

## ----fredr_source_releases2---------------------------------------------------
fredr_source_releases(
  source_id = 14L,
  realtime_start = as.Date("1950-01-01")
)

