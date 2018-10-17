rm(list = ls())

library(isocyanate)
library(dplyr)
library(magrittr)
library(purrr)
library(stats)
library(zoo)
library(readr)

VIX <- read_csv("data-raw/VIX.csv", na = 'n/a') %>% na.omit %>%
  {xts::xts(.[, 2:ncol(.)], as.Date(as.character(.$Date), "%m/%d/%y"))}

columns <- c('open', 'high', 'low', 'close', 'adjclose', 'changed', 'changep', 'volume')
# file.names <- sapply(c(columns, 'rollmean', 'rollvcov'), function(x) paste('data-raw/', "PENDING", x, '.csv', sep = ''))

factors <- c("MTUM", "QUAL", "VLUE", "SIZE", "USMV", "SPY")
fac.initset <- GenInitSet(factors, columns, as.Date("2013-01-01"), list('VIX' = VIX)) # 1031, approx 5 years
fac.varset <- GenVarSet(columns, fac.initset, factors) # exclude VIX in the grand list

sectors <- c("XLB", "XLI", "XLY", "XLP", "XLE", "XLF", "XLU", "XLV", "XLK", "SPY")
sec.initset <- GenInitSet(sectors, columns, as.Date("1998-01-01"), list('VIX' = VIX)) # 4283, approx 20 years
sec.varset <- GenVarSet(columns, sec.initset, sectors)

save(fac.initset, fac.varset, file = "data/factors.rda")
save(sec.initset, sec.varset, file = "data/sectors.rda")
