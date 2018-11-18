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
load("../data/sectors.rda") # use sectors to get maximum period
logret <- log(sec.varset$changep + 1) # change to log returns
rollregres <- logret %>% RollingRegres(21, 'changep.SPY') # pca1 <- prcomp(rollregres[, 1:9], scale. = TRUE)
# rollalpha <- logret %>% RollingAlpha(21, 'changep.SPY') # acf.outputs <- rollalpha %>% acf
# CONC: pretty stable variance of regres when applying different length of window
# CONC: alphas have high auto-corr, low cross-sectional auto-corr  # CONC: should model alpha shocks

ret.xlk <- logret$changep.XLK %>% CutSeriesQuantile # TECH
ret.xly <- logret$changep.XLY %>% CutSeriesQuantile # CD
ret.xlf <- logret$changep.XLF %>% CutSeriesQuantile # FIN
ret.xli <- logret$changep.XLI %>% CutSeriesQuantile # IND 
ret.xlv <- logret$changep.XLV %>% CutSeriesQuantile # HC 
ret.spy <- logret$changep.SPY %>% CutSeriesQuantile 

funcs <- list("mom" = sum, "sd" = sd, "dsd" = function(x) sd(x[x<0]), "asd" = function(x) sd(x[x<0])-sd(x[x>0]))
periods <- list("2w" = 10, "1m" = 21, "3m" = 21 * 3)
n.top <- 3 # used for reportting top heights

# XLK: spy.mom.3m:18, xlk.mom.3m:20, xlk.sd.3m:24, xlk.dsd.1m:22, xlk.asd.3m:18
target <- ret.xlk
core <- ret.spy; core.name <- "spy" 
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 
core <- ret.xlk; core.name <- "xlk"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 

# XLY: spy.mom.3m:17, xly.mom.3m:12, xly.sd.2w:15
target <- ret.xly
core <- ret.spy; core.name <- "spy";
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 
core <- ret.xly; core.name <- "xly"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 

# XLF: spy.mom.3m:20, spy.sd.2w:23, xlf.mom.3m:21, xlf.sd.2w:27, xlf.dsd.3m:23
target <- ret.xlf
core <- ret.spy; core.name <- "spy" 
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 
core <- ret.xlf; core.name <- "xlf"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 

# XLI: spy.mom.3m:19, xli.mom.3m:21, xli.sd.2w:17, xli.dsd.2w:14
target <- ret.xli
core <- ret.spy; core.name <- "spy" 
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 
core <- ret.xli; core.name <- "xlf"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 

# XLV: spy.mom.3m:12, xlv.mom.2w:10, xlv.sd.2w:13, xlv.dsd.2w:11
target <- ret.xlv
core <- ret.spy; core.name <- "spy" 
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 
core <- ret.xlv; core.name <- "xlv"
ForgeBullets(core, core.name, funcs, periods) %>% lapply(GenHC, target %>% lag.xts(-1)) %>% 
  lapply(function(hc) hc$height %>% {.[(length(.)-(n.top-1)):length(.)]}) %>%
  {do.call(cbind, .)} %>% signif(3) * 100 # SHOOT 

# CONC: common: spy.mom.3m + private: mom.3m, sd.2w
# if n.group large (20), height increase, but some from sampling noise

