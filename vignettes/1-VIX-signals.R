# CUSTOMIZED VIX SIGNAL TESTS #

rm(list = ls())

library(dplyr)
library(DRIVS)
library(isocyanate)
library(magrittr)
library(MASS)
library(stats)
library(xts)

set.seed(12896)
load("data/sectors.rda")
period <- "2005/2018"

ret <- log(sec.initset$SPY[, 'changep'] + 1)[period]; VIX.full <- sec.initset$VIX[period]
risk <- ret %>% rollapply(10, sd, align = 'left'); VIX <- VIX.full[, 'Close']
VIX.sr <- VIX.full %$% {High - Low} %>% rollapply(5, sum) %>% na.omit # sum of range
VIX.qv <- VIX.full %$% {abs(Close - Open) + abs(Open - lag.xts(Close))} %>% rollapply(10, sum) %>% na.omit

vars <- merge(risk, VIX, VIX.sr, VIX.qv) %>% `names<-`(c('Risk', 'VIX', 'VarVIX', 'VarVIX2')) %>% na.omit
summary(lm(Risk ~ VIX + VarVIX + VarVIX2, data = vars))
# sub.vars <- vars %$% {vars[VIX > 15]} # summary(lm(Risk ~ VIX + VarVIX + VarVIX2, data = sub.vars))

VIX.sr.qt <- quantile(vars$VarVIX, 0.5); VIX.qv.qt <- quantile(vars$VarVIX2, 0.5)
idx <- vars[(vars$VIX > 10) & (vars$VarVIX > VIX.sr.qt) & (vars$VarVIX2 > VIX.qv.qt)] %>% index
bin.signal <- ret %>% rollapply(5, function(df) !any(index(df) %in% as.numeric(idx)), by.column = F) %>% lag.xts %>% na.omit
ret.p <- ret[10:length(ret)] * bin.signal
GenCoreMetrics(ret, period = 'Daily')[1]; exp(cumsum(ret)) %>% plot(type = 'l')
GenCoreMetrics(ret.p, period = 'Daily')[1]; exp(cumsum(ret.p)) %>% plot(type = 'l')
  # Mean         Stdev  Downside Dev        Sharpe       Sortino % of Positive         MaxDD
  # 7.0276844    19.5725801    20.9341263     0.3590576     0.3357047    54.3378995    53.3291886
  # 2.8139081     7.4726867    11.5200865     0.3765591     0.2442610    26.1694472    15.2728597
