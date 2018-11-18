# FUNCTIONALITIES DEMONSTRATED IN THIS SCRIPT WILL BE PACKED INTO THE PKG

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

load("../data/sectors.rda")
logret <- log(sec.varset$changep + 1)

# Stage II: Multiasset Copula Estimation ====
winlen <- 5 # fix a week # different length, have different infomation,
  # 5 days and 10 days are pretty much the same, combine?
ret.spy <- logret$changep.SPY
quantiles <- ret.spy %>% zoo::rollapply(winlen, sum) %>% na.omit %>% GenEmpQuantileVec
  # numeric signal, no need to care about index at this moment

n.group <- 10; cuts <- seq(0, 1, 1/n.group)
  # cuts <- c(0, 1/10, 3/10, 0.5, 1-3/10, 1-1/10, 1) # cuts <- seq(0, 1, 1/n.group)
bools <- lapply(1:n.group, function(i) {
  (quantiles < cuts[i+1] & quantiles > cuts[i]) %>%
    xts::xts(zoo::index(quantiles)) %>% xts::lag.xts(1) # lag NOW!
}) # bool 2d signals with same index as the numeric signal and MIGHT HAVE NAs
  # bools %>% lapply(xts::periodicity)
  # xts::periodicity(quantiles) # should equal to above

bw <- 0.003; gs <- 128L
ret.xlk <- logret$changep.XLK # 1, (2, 3, 4, 10), (5, 6, 7, 8, 9)
groups.xlk <- lapply(bools, function(bool) {
  xts::merge.xts(ret.xlk, bool) %>% na.omit() %>%
    `colnames<-`(c('ret', 'idx')) %>% {.$ret[.$idx != 0]}
})
ret.xly <- logret$changep.XLY # 1, (2, 3, 4, 10), (5, 6, 7, 8, 9)
groups.xly <- lapply(bools, function(bool) {
  xts::merge.xts(ret.xly, bool) %>% na.omit() %>%
    `colnames<-`(c('ret', 'idx')) %>% {.$ret[.$idx != 0]}
})
ret.xlf <- logret$changep.XLF # 1, (2, 3, 4, 10), (5, 6, 7, 8, 9)
groups.xlf <- lapply(bools, function(bool) {
  xts::merge.xts(ret.xlf, bool) %>% na.omit() %>%
    `colnames<-`(c('ret', 'idx')) %>% {.$ret[.$idx != 0]}
})

# NOTE: do not cut the signal using 0.1% quantiles here, match multiassets
# groups.xlk %>% lapply(dim); groups.xly %>% lapply(dim) # should be identical

# Copula simulation: https://datascienceplus.com/modelling-dependence-with-copulas/
i <- 1 # the first condition
groups <- list(groups.xlk, groups.xly, groups.xlf)
cond.rets <- groups %>% lapply(function(x) x[[i]]) %>% do.call(cbind, .) %>% `colnames<-`(c("XLK", "XLY", "XLF"))
cond.qtls <- lapply(1:ncol(cond.rets), function(i) GenEmpQuantileVec(cond.rets[, i])) %>%
  Reduce(xts::merge.xts, .) %>% `colnames<-`(c("XLK", "XLY", "XLF"))
cond.pobs <- VineCopula::pobs(as.matrix(cond.rets))
scatter.smooth(cond.qtls[, 1], cond.qtls[, 2])

sapply(1:(ncol(cond.qtls)-1), function(i) {
  sapply((i+1):ncol(cond.qtls), function(j) {
    print(c(i, j))
    print(BiCopSelect(cond.pobs[, i], cond.pobs[, j], familyset=NA))
  })
}) # all pick t copula

# MANUAL SET
# sigma <- cor(cond.qtls) # sigma
# z <- mvrnorm(n, mu = rep(0, 3), Sigma=sigma,empirical=T)
tcop.coef <- tCopula(dim = 3) %>% fitCopula(cond.pobs, method='ml') %>% coef
tcop <- tCopula(dim = 3, param = tcop.coef[1], df = tcop.coef[2])
# persp(tcop, dCopula) # dim 2

u <- rCopula(50000, tcop)
# plot(u[, 1], u[, 3], pch='.', col='blue'); cor(u, method='spearman')
sim <- lapply(1:ncol(u), function(i){
  as.vector(sapply(u[, i], function(x) quantile(cond.rets[, i], x)))
  # potential improvement: used smoothed distributions
}) %>% do.call(cbind, .)

par(mfrow = c(3, 1))
for (i in 1:3) {cond.rets[, i] %>% hist(breaks = 300)}
for (i in 1:3) {sim[, i] %>% hist(breaks = 300)}

write.table(sim, file = "data-raw/sample.csv", col.names = F, row.names = FALSE, sep=',')
  # write.csv will include colnames
