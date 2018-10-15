rm(list = ls())

library(dplyr)
library(magrittr)
library(purrr)
library(stats)
library(zoo)

tickers <- c("MTUM", "QUAL", "VLUE", "SIZE", "USMV", "SPY")
columns <- c('open', 'high', 'low', 'close', 'adjclose',
             'changep', 'volume', 'tradeval', 'tradevol')
file.names <- sapply(c(columns, 'rollmean', 'rollvcov'), function(x) paste('data-raw/SF_', x, '.csv', sep = ''))

var.list <- tickers %>% map(function(x) GetXTS(x, "2013-01-01") %>% `[`(, columns)) %>% `names<-`(tickers)
  # factor ETFs all have inception date after 2013-01-01, take the maximum period, end date set today
  # sample size higher than 1000 would be enough, approximately 5 years, we can do weekly rebalancing

vars.xts <- lapply(columns, function(name) {
  do.call(cbind, lapply(var.list, function(df) df[, name])) %>%
    `colnames<-`(lapply(tickers, function(x) paste(name, x, sep = '')))
}) %>% `names<-`(columns)

vars.xts$rollmean <- vars.xts$changep %>% rollapply(21, mean) %>% na.omit
vars.xts$rollvcov <- vars.xts$changep %>% GenVCOV('daily', VecVCOV, 21)

vars.tbl <- vars.xts %>% map(tibbleXTS)

map(seq_along(vars.tbl), function(i) write.csv(vars.tbl[[i]], file.names[i], row.names=FALSE))

save(vars.tbl, file = "data/Pop_variables.rda")

