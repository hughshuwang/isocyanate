# STANDARD EDE-BASED ret SIGNAL RESEARCH PROCEDURE #

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
load("./data/sectors.rda") # use sectors to get maximum period
ret <- log(sec.varset$changep + 1) # change to log returns
colnames(ret) <- colnames(ret) %>% lapply(function(x) strsplit(x, split = '[.]')[[1]][2]) %>% unlist
ret <- apply(ret, 2, CutSeriesQuantile) %>% {xts(., rownames(.) %>% as.Date)}

# rollregres <- ret %>% RollingRegres(21, 'SPY') # pca1 <- prcomp(rollregres[, 1:9], scale. = TRUE)
# rollalpha <- ret %>% RollingAlpha(21, 'SPY') # acf.outputs <- rollalpha %>% acf
# CONC: pretty stable variance of regres when applying different length of window
# CONC: alphas have high auto-corr, low cross-sectional auto-corr  # CONC: should model alpha shocks

funcs <- list("mom" = sum, "sd" = sd, "dsd" = function(x) sd(x[x<0]), "asd" = function(x) sd(x[x<0])-sd(x[x>0]))
periods <- list("2w" = 10, "1m" = 21, "3m" = 21 * 3)
n.top <- 3 # used for reportting top heights

# XLK: SPY.mom.3m:18, XLK.mom.3m:20, XLK.sd.3m:24, XLK.dsd.1m:22, XLK.asd.3m:18
target <- ret$XLK
core <- ret$SPY; core.name <- "SPY"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT
core <- ret$XLK; core.name <- "XLK"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT

# XLY: SPY.mom.3m:17, XLY.mom.3m:12, XLY.sd.2w:15
target <- ret$XLY
core <- ret$SPY; core.name <- "SPY";
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT
core <- ret$XLY; core.name <- "XLY"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT

# XLF: SPY.mom.3m:20, SPY.sd.2w:23, XLF.mom.3m:21, XLF.sd.2w:27, XLF.dsd.3m:23
target <- ret$XLF
core <- ret$SPY; core.name <- "SPY"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT
core <- ret$XLF; core.name <- "XLF"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT

# XLI: SPY.mom.3m:19, XLI.mom.3m:21, XLI.sd.2w:17, XLI.dsd.2w:14
target <- ret$XLI
core <- ret$SPY; core.name <- "SPY"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT
core <- ret$XLI; core.name <- "XLF"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT

# XLV: SPY.mom.3m:12, XLV.mom.2w:10, XLV.sd.2w:13, XLV.dsd.2w:11
target <- ret$XLV
core <- ret$SPY; core.name <- "SPY"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT
core <- ret$XLV; core.name <- "XLV"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>%
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT

# CONC: common: SPY.mom.3m + private: mom.3m, sd.2w
# if n.group large (20), height increase, but some from sampling noise

# TODO: write a function to get a ranking long format table for signals
