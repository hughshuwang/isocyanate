# STANDARD EDE-BASED LOGRET SIGNAL RESEARCH PROCEDURE #

rm(list = ls())

library(ash)
library(copula)
library(dplyr)
library(isocyanate)
library(KernSmooth)
library(magrittr)
library(MASS)
library(purrr)
library(readr)
library(stats)
library(VineCopula)
library(zoo)

set.seed(12896)
load("data/sectors.rda") # use sectors to get maximum period
logret <- log(sec.varset$changep + 1) # change to log returns
period <- "1999/2017" # TEST CONSISTENCY

ret.xlk <- logret$changep.XLK[period] %>% CutSeriesQuantile
ret.spy <- logret$changep.SPY[period] # fix a week # different length, have different infomation, 5-10 pretty same

mom2w <- ret.spy %>% zoo::rollapply(10, sum) %>% na.omit %>% GenEmpQuantileVec
mom1m <- ret.spy %>% zoo::rollapply(21, sum) %>% na.omit %>% GenEmpQuantileVec
sd2w <- ret.spy %>% zoo::rollapply(10, sd) %>% na.omit %>% GenEmpQuantileVec
sd1m <- ret.spy %>% zoo::rollapply(21, sd) %>% na.omit %>% GenEmpQuantileVec
dsd2w <- ret.spy %>% zoo::rollapply(10, function(x) sd(x[x < 0])) %>% na.omit %>% GenEmpQuantileVec
dsd1m <- ret.spy %>% zoo::rollapply(21, function(x) sd(x[x < 0])) %>% na.omit %>% GenEmpQuantileVec
asym1m <- ret.spy %>% zoo::rollapply(21, function(x) sd(x[x < 0]) - sd(x[x > 0])) %>% na.omit %>% GenEmpQuantileVec
asym3m <- ret.spy %>% zoo::rollapply(21, function(x) sd(x[x < 0]) - sd(x[x > 0])) %>% na.omit %>% GenEmpQuantileVec
# CONC: pretty stable variance of regres when applying different length of window
# CONC: alphas have high auto-corr, low cross-sectional auto-corr  # CONC: should model alpha shocks
# TODO: build a list of signals, using pack and same naming convention

signal <- list(asym1m); target <- ret.xlk %>% xts::lag.xts(-1)
bools <- signal %>% lapply(function(signal) signal %>% GenBoolSignal(n.group = 9)) # cuts = c(0, 1/10, 3/10, 0.5, 1-3/10, 1-1/10, 1))
groups <- target %>% na.omit %>% CutSeriesQuantile %>% GenCondGroups(bools) # lapply(., function(x) hist(x, breaks = 200, xlim = c(-0.1, 0.1)))
dens <- GenBKDE(groups, bw = 0.002, gs = 128) # dens %>% lapply(., function(x) plot(x, type = 'l'))
# TODO: get the index out at the same time, each density should have 2 columns, then to double check
# bbands <- GenBKDEBand(groups, bw = 0.003, n = 1000); bbnoise <- lapply(bbands, function(df) df[, 2] - df[, 1]) # bb std

odens <- GenBKDE(ret.xlk)[[1]]; range <- range(ret.xlk)
idx <- seq(from = range[1], to = range[2], length.out = length(odens)) # cbind(idx, odens[[1]])
dmat <- sapply(1:length(groups), function(i) {sapply(1:length(groups), function(j) {DistUR(dens, i, j, idx)})}) %>% as.dist
hc <- hclust(dmat*100, method = "complete")
hc$height %>% {.[(length(.)-2):length(.)]}
# if n.group large (20), height increase, but some from sampling noise
# TODO: write a signal diagnosis module
