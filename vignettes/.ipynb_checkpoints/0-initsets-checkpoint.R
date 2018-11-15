rm(list = ls())

library(isocyanate)
library(dplyr)
library(magrittr)
library(purrr)
library(stats)
library(zoo)
library(readr)

setwd("/home/hugh/isocyanate")

VIX <- read_csv("./data-raw/VIX.csv", na = 'n/a') %>% na.omit %>%
  {xts::xts(.[, 2:ncol(.)], as.Date(as.character(.$Date), "%m/%d/%y"))}

columns <- c('open', 'high', 'low', 'close', 'adjclose', 'changed', 'changep', 'volume')
# file.names <- sapply(c(columns, 'rollmean', 'rollvcov'), function(x) paste('data-raw/', "PENDING", x, '.csv', sep = ''))

factors <- c("MTUM", "QUAL", "VLUE", "SIZE", "USMV", "SPY")
fac.initset <- GenInitSet(factors, columns, as.Date("2013-01-01"), list('VIX' = VIX)) # 1031, approx 5 years
fac.varset <- GenVarSet(columns, fac.initset, factors) # exclude VIX in the grand list

sectors <- c("XLB", "XLI", "XLY", "XLP", "XLE", "XLF", "XLU", "XLV", "XLK", "SPY")
sec.initset <- GenInitSet(sectors, columns, as.Date("1998-01-01"), list('VIX' = VIX)) # 4283, approx 20 years
sec.varset <- GenVarSet(columns, sec.initset, sectors)

save(fac.initset, fac.varset, file = "./data/factors.rda")
save(sec.initset, sec.varset, file = ",.data/sectors.rda")

fac.initset$SPY %>% tibbleXTS %>% write_csv('./data-raw/SPY.csv')

# COMMENTS:
# `initset` has all time series including tradable assets and indicators, each element is an asset or indicator
# `varset` has all variables available for tradable assets, each element is a variable with tradable assets being columns
# Two sets both have identical time indexes for all members.

# ADDITIONAL VARIABLES (PENDING)
# vars.xts$rollmean <- vars.xts$changep %>% rollapply(21, mean) %>% na.omit
# vars.xts$rollvcov <- vars.xts$changep %>% GenVCOV('daily', VecVCOV, 21)
# vars.tbl <- vars.xts %>% map(tibbleXTS)
# map(seq_along(vars.tbl), function(i) write.csv(vars.tbl[[i]], file.names[i], row.names=FALSE))
#
# do.call(cbind, lapply(var.list, function(df) df[, 'changep'])) %>% cor %>% round(3)
# # TODO: test the corr across rolling alphas of factors and sectors
