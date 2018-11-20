# CORR MODELING AND COPULA SIMULATIONS #

rm(list = ls())

library(isocyanate)
library(magrittr)
library(xts)
library(zoo)

set.seed(12896)
load("../data/sectors.rda")
logret <- log(sec.varset$changep + 1)

funcs <- list("mom" = sum, "sd" = sd)
periods <- list("2w" = 10, "1m" = 21, "3m" = 21 * 3)

ret.xlk <- logret$changep.XLK %>% CutSeriesQuantile # TECH
ret.xly <- logret$changep.XLY %>% CutSeriesQuantile # CD
ret.xlf <- logret$changep.XLF %>% CutSeriesQuantile # FIN
ret.xli <- logret$changep.XLI %>% CutSeriesQuantile # IND 
ret.xlv <- logret$changep.XLV %>% CutSeriesQuantile # HC 
ret.spy <- logret$changep.SPY %>% CutSeriesQuantile 

bullets.spy <- ForgeBullets(ret.spy, "spy", funcs, periods)
bullets.xlk <- ForgeBullets(ret.xlk, "xlk", funcs, periods)
bullets.xly <- ForgeBullets(ret.xly, "xly", funcs, periods)
bullets.xlf <- ForgeBullets(ret.xlf, "xlf", funcs, periods)
bullets.xli <- ForgeBullets(ret.xli, "xli", funcs, periods)
bullets.xlv <- ForgeBullets(ret.xlv, "xlv", funcs, periods)

shlist.spy.xlk <- bullets.spy %>% lapply(GenHC, ret.xlk %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))
shlist.xlk.xlk <- bullets.xlk %>% lapply(GenHC, ret.xlk %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))
shlist.spy.xly <- bullets.spy %>% lapply(GenHC, ret.xly %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))
shlist.xly.xly <- bullets.xly %>% lapply(GenHC, ret.xly %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))
shlist.spy.xlf <- bullets.spy %>% lapply(GenHC, ret.xlf %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))
shlist.xlf.xlf <- bullets.xlf %>% lapply(GenHC, ret.xlf %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))
shlist.spy.xli <- bullets.spy %>% lapply(GenHC, ret.xli %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))
shlist.xli.xli <- bullets.xli %>% lapply(GenHC, ret.xli %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))
shlist.spy.xlv <- bullets.spy %>% lapply(GenHC, ret.xlv %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))
shlist.xlv.xlv <- bullets.xlv %>% lapply(GenHC, ret.xlv %>% lag.xts(-1)) %>% 
    lapply(function(x) GenBinShHC(x$merge))

# COMMON: spy.mom.3m # all lower 10%
shlist.spy.xlf$spy.mom.3m
shlist.spy.xli$spy.mom.3m
shlist.spy.xly$spy.mom.3m
shlist.spy.xlk$spy.mom.3m
shlist.spy.xlv$spy.mom.3m
# TODO: Merge cuts

list(bullets.spy, bullets.xlf, bullets.xli, bullets.xlk, bullets.xlv, bullets.xly) %>% {do.call(cbind, .)}

sh.dens <- GenBoolSignal(bullets.spy$`spy.mom.3m`, 9) %>% {GenCondGroups(ret.xlk %>% lag.xts(-1), .)} %>% {GenBKDE(.)$dens} %>% GenBinShDens(shlist.spy.xlk$`spy.mom.3m`)
# shrinked group densities

# TODO: Gen the group we are currently in
# TODO: Output the density for a given day
# TODO: Estimate conditional corr copula structure
# TODO: Simulate using conditional corr and overall empirical CDF
# TODO: export sample

# OUTPUT
png(filename = "../images/tmp.png")
plot(sh.dens[[2]], type = 'l')
invisible(dev.off())

