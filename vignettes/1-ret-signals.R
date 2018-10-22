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

quantiles <- ret.spy %>% zoo::rollapply(10, sum) %>% na.omit %>% GenEmpQuantileVec # num signal

  # the rolling window has some shortcomings, lagged responce, not robust, not sensitive to abnormalies
  # CONC: pretty stable variance of regres when applying different length of window
  # CONC: alphas have high auto-corr, low cross-sectional auto-corr
  # CONC: should model alpha shocks
  # TODO: use olhc to get another kind of beta, but won't be much different

# bools <- GenBoolSignal(quantiles, n.group = 6, cuts = c(0, 1/10, 3/10, 0.5, 1-3/10, 1-1/10, 1))
bools <- GenBoolSignal(quantiles, n.group = 9) # signals

groups <- ret.xlk %>% CutSeriesQuantile %>% GenCondGroups(bools) # grouping
  # conditional distribution var # for XLK, SPY 5days cumret: 1, (2, 3, 4, 10), (5, 6, 7, 8, 9)
  # par(mfrow = c(3, 2)); groups %>% lapply(., function(x) hist(x, breaks = 200, xlim = c(-0.1, 0.1)))

par(mfrow = c(3, 3))
dens <- GenBKDE(groups, bw = 0.002, gs = 128); dens %>% lapply(., function(x) plot(x, type = 'l'))
# bbands <- GenBKDEBand(groups, bw = 0.003, n = 1000); bbnoise <- lapply(bbands, function(df) df[, 2] - df[, 1]) # bb std

odens <- GenBKDE(ret.xlk)[[1]]; range <- range(ret.xlk)
idx <- seq(from = range[1], to = range[2], length.out = length(odens)) # cbind(idx, odens[[1]])
dmat <- sapply(1:length(groups), function(i) {sapply(1:length(groups), function(j) {DistUR(dens, i, j, idx)})}) %>% as.dist
hc <- hclust(dmat*100, method = "complete"); plot(hc); hc$height # if n.group large (20), height increase, but some from sampling noise
  # spotify high/low volatility situation # decision making for cutting conditions: eyeballing for now
