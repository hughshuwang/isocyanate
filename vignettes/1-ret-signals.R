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

# Stage I: EDE-based Signal Selection ----
ret.spy <- logret$changep.SPY; winlen <- 5 # fix a week # different length, have different infomation, 5-10 pretty same
quantiles <- ret.spy %>% zoo::rollapply(winlen, sum) %>% na.omit %>% GenEmpQuantileVec # num signal

# bools <- GenBoolSignal(quantiles, n.group = 6, cuts = c(0, 1/10, 3/10, 0.5, 1-3/10, 1-1/10, 1))
bools <- GenBoolSignal(quantiles, n.group = 10)
groups <- logret$changep.XLK %>% CutSeriesQuantile %>% GenCondGroups(bools)
  # conditional distribution var # for XLK, SPY 5days cumret: 1, (2, 3, 4, 10), (5, 6, 7, 8, 9)
  # par(mfrow = c(3, 2)); groups %>% lapply(., function(x) hist(x, breaks = 200, xlim = c(-0.1, 0.1)))

dens <- GenBKDE(groups, bw = 0.002, gs = 128); dens %>% lapply(., function(x) plot(x, type = 'l'))
# bbands <- GenBKDEBand(groups, bw = 0.003, n = 1000); bbnoise <- lapply(bbands, function(df) df[, 2] - df[, 1]) # bb std

sumsq <- function(dens, i, j) {sum((dens[[i]] - dens[[j]]) ^ 2)} # distance func
adjsq <- function(){} # adjusted measure for distance, give more weights
dmat <- sapply(1:length(groups), function(i) {sapply(1:length(groups), function(j) {sumsq(dens, i, j)})}) %>% as.dist

hc <- hclust(dmat, method = "average"); plot(hc); hc$height # if n.group large (20), height increase, but some from sampling noise
  # spotify high/low volatility situation
  # decision making for cutting conditions: eyeballing for now

  # estimate density of sample, and formulate the right output
  # TODO: plot outputs to show results, pick different time period to TEST CONSISTENCY
  # TODO: define new measure that can give tail more weights
  # TODO: formulate an out-of-sample test of fitting the distribution compared with
  # TODO: fit a CRRA utility, and write a new measure for this adjustment (mind the sample size)



