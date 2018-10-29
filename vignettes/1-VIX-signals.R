# CUSTOMIZED VIX SIGNAL TESTS #

rm(list = ls())

library(dplyr)
library(isocyanate)
library(magrittr)
library(MASS)
library(stats)
library(xts)

set.seed(12896)
load("data/sectors.rda")
period <- "2005/2018"

# Sectors: defensive: XLV, XLU, XLP, aggressive: XLK, XLF
ret.spy <- log(sec.initset$SPY[, 'changep'] + 1)[period]
ret.agg <- sec.initset %$% {XLF[, 'changep']} %>% {log(.+1)[period]}
ret.def <- sec.initset %$% {XLP[, 'changep']} %>% {log(.+1)[period]}
  # can add more sectors in and take mkcap avg

VIX.full <- sec.initset$VIX[period]
VIX    <- VIX.full[, 'Close'] %>% `names<-`('VIX')
VIX.qv <- VIX.full %$% {abs(Close - Open) + abs(Open - lag.xts(Close))} %>% rollapply(10, sum) %>% na.omit %>% `names<-`('VIX.qv')
VIX.mm <- VIX.full %$% {High - Low} %>% rollapply(5, sum) %>% na.omit %>% `names<-`('VIX.mm') # max-min
VIX.co <- VIX.full %$% {abs(Close - Open)} %>% rollapply(5, sum) %>% na.omit %>% `names<-`('VIX.co') # abs close-open
VIX.sr <- (VIX.mm + VIX.co) %>% `names<-`('VIX.sr') # one week variation of VIX used for next week volatility
  # TODO: decide the cond distrs for VIX.co and VIX.sr

risk <- ret.agg %>% rollapply(10, sd, align = 'left') %>% `names<-`('risk')
risk <- ret.agg %>% rollapply(10, function(vec) sd(vec[vec < 0.01]), align = 'left') %>% `names<-`('risk')
vars <- merge(risk, VIX, VIX.mm, VIX.co, VIX.qv, VIX.sr) %>% na.omit
summary(lm(risk ~ VIX.co, data = vars)) # can add conditions on VIX

idx <- vars[(vars$VIX > 10) & (vars$VIX.sr > quantile(vars$VIX.sr, 0.5))] %>% index # VIX is not very useful if cond > 10
bin.signal <- ret.spy %>% rollapply(5, function(df) !any(index(df) %in% as.numeric(idx)), by.column = F) %>% lag.xts %>% na.omit
ret.p <- ret.agg[10:length(ret.agg)] * bin.signal + ret.def[10:length(ret.def)] * !bin.signal
rbind(GenCoreMetrics(ret.spy, period = 'Daily')[[1]] %>% round(2), GenCoreMetrics(ret.p, period = 'Daily')[[1]] %>% round(2))[, 1:8] %>% `row.names<-`(c('SPY', 'New'))
exp(cumsum(ret.p)) %>% plot(type = 'l', ylim = c(0, 6)); exp(cumsum(ret.spy)) %>% lines(col = 'blue')

  # Scenario: sr higher than 0.5 quantile, XLF vs XLP
  # Sig  Mean Stdev Downside Dev Sharpe Sortino % of Positive MaxDD  Worst
  # SPY  7.03 19.57        20.93   0.36    0.34         54.34 53.33 -10.16
  # New 12.03 15.72        16.42   0.76    0.73         54.66 29.86  -5.69
  #     +70%  -20%          -22%  +110%    +114%               -44%


