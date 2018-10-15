rm = list(ls())

library(isocyanate)
library(magrittr)
library(purrr)
library(xts)

# Stage I: data fetching ----

etf.list <- c("SPY", "XLB", "XLI", "XLY", "XLP", "XLE", "XLF", "XLU", "XLV", "XLK")
sub.list <- c()

var.list <- lapply(etf.list, function(symbol) {
  GetXTS(symbol, as.Date("1998-01-01"), as.Date("2018-10-12"))[, 1:8]
}) %>% CleanSubset %>% `names<-`(etf.list) # same index

var.list %>% lapply(xts::periodicity)

save(var.list, file = "data/sectors.rda")

# Stage II: grouping
# TODO: time index mgmt
# TODO:


