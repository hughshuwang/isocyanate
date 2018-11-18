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

rollregres <- logret %>% RollingRegres(21, 'changep.SPY') # pca1 <- prcomp(rollregres[, 1:9], scale. = TRUE)
rollalpha <- logret %>% RollingAlpha(21, 'changep.SPY') # acf.outputs <- rollalpha %>% acf
# CONC: pretty stable variance of regres when applying different length of window
# CONC: alphas have high auto-corr, low cross-sectional auto-corr  # CONC: should model alpha shocks

ret.xlk <- logret$changep.XLK %>% CutSeriesQuantile
ret.spy <- logret$changep.SPY %>% CutSeriesQuantile # NOT PERIOD SUBSETTING

periods <- list("2w" = 10, "1m" = 21, "3m" = 21 * 3)
funcs <- list("mom" = sum, "sd" = sd, "dsd" = function(x) sd(x[x<0]), 
              "asd" = function(x) sd(x[x<0])-sd(x[x>0]))

bullets <- ForgeBullets(ret.spy, "spy", funcs, periods) # deparse(substitute(ret.spy)) %>% strsplit(split = '[.]')[1]
bullets %>% lapply(GenHeights, target = ret.xlk %>% lag.xts(-1)) %>% {do.call(cbind, .)} %>% signif(3) # SHOOT 


# TODO: output cluster information
GenHCList <- function(hc.merge, n.group) {
    hc.merge <- hc$merge; n.group <- 4

    clusters <- vector()
    n <- nrow(hc.merge)
    crow <- hc.merge[n, ]

    append(clusters, -crow[crow < 0])
}

# if n.group large (20), height increase, but some from sampling noise
# TODO: write a signal diagnosis module



