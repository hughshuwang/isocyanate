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
library(xts)
library(zoo)

set.seed(12896)
period <- "1999/2017" # TEST CONSISTENCY

load("../data/sectors.rda") # use sectors to get maximum period
logret <- log(sec.varset$changep + 1) # change to log returns

ret.xlk <- logret$changep.XLK %>% CutSeriesQuantile
ret.spy <- logret$changep.SPY %>% CutSeriesQuantile # NOT PERIOD SUBSETTING

mom2w <- ret.spy %>% rollapply(10, sum) %>% na.omit %>% GenEmpQuantileVec
mom1m <- ret.spy %>% rollapply(21, sum) %>% na.omit %>% GenEmpQuantileVec
sd2w <- ret.spy %>% rollapply(10, sd) %>% na.omit %>% GenEmpQuantileVec
sd1m <- ret.spy %>% rollapply(21, sd) %>% na.omit %>% GenEmpQuantileVec
dsd2w <- ret.spy %>% rollapply(10, function(x) sd(x[x<0])) %>% na.omit %>% GenEmpQuantileVec
dsd1m <- ret.spy %>% rollapply(21, function(x) sd(x[x<0])) %>% na.omit %>% GenEmpQuantileVec
asym1m <- ret.spy %>% rollapply(21, function(x) sd(x[x<0])-sd(x[x>0])) %>% na.omit %>% GenEmpQuantileVec
asym3m <- ret.spy %>% rollapply(21 * 3, function(x) sd(x[x<0])-sd(x[x>0])) %>% na.omit %>% GenEmpQuantileVec
# CONC: pretty stable variance of regres when applying different length of window
# CONC: alphas have high auto-corr, low cross-sectional auto-corr  # CONC: should model alpha shocks

list(mom2w, mom1m, sd2w, sd1m, dsd2w, dsd1m, asym1m, asym3m) %>% 
    lapply(GenHeights, target = ret.xlk %>% lag.xts(-1)) %>% 
    {do.call(cbind, .)} # keep name conventions


GenHCList <- function(hc.merge, n.group) {
    hc.merge <- hc$merge; n.group <- 4

    clusters <- vector()
    n <- nrow(hc.merge)
    crow <- hc.merge[n, ]

    append(clusters, -crow[crow < 0])
}

# if n.group large (20), height increase, but some from sampling noise
# TODO: write a signal diagnosis module
