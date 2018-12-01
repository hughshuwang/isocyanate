# CORR MODELING AND COPULA SIMULATIONS #

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

funcs <- list("mom" = sum, "sd" = sd)
periods <- list("2w" = 10, "1m" = 21, "3m" = 21 * 3)
bullets <- lapply(1:ncol(ret), function(i)
  ForgeBullets(ret[, i], colnames(ret)[i], funcs, periods)) %>%
  `names<-`(colnames(ret))

# Correlation Structure Bools
shlists <- 1:ncol(ret) %>% lapply(function(i) {
  shlist <- bullets$SPY %>%
    lapply(GenHC, ret[, i] %>% lag.xts(-1)) %>%
    lapply(function(x) GenBinShHC(x$merge))
  shlist$SPY.mom.3m
}) # XLK, XLY, XLF, XLI, XLV # shoot in batch
# tmp %>% unlist %>% unique %>% sort
mat <- ShlistToGroupMat(shlists[[1]]) # multiply matrixes
shlist <- GroupMatToShlist(mat) # shlists[[1]]

bools.cm <- bullets$SPY$SPY.mom.3m %>% GenBoolSignal(n.group = 9) # cuts = c(0, 1/10, 3/10, 0.5, 1-3/10, 1-1/10, 1))
bools.cm <- lapply(1:length(shlist), function(i) bools.cm[shlist[[i]]] %>%
                     do.call(cbind, .) %>% apply(1, any) %>% xts(as.Date(names(.))))

tcops <- lapply(1:length(bools.cm), function(i) {
  # REF: https://datascienceplus.com/modelling-dependence-with-copulas/
  cond.rets <- ret[, c('XLK', 'XLY', 'XLF')] %>%
    lapply(GenCondGroups, bools=bools.cm) %>%
    lapply(function(x) x[[i]]) %>% do.call(cbind, .) %>%
    `colnames<-`(c("XLK", "XLY", "XLF"))
  cond.qtls <- lapply(1:ncol(cond.rets), function(i) GenEmpQuantileVec(cond.rets[, i])) %>%
    Reduce(xts::merge.xts, .) %>% `colnames<-`(c("XLK", "XLY", "XLF")) # conditional quantiles
  cond.pobs <- VineCopula::pobs(as.matrix(cond.rets)) # conditional pobs
  # scatter.smooth(cond.qtls[, 1], cond.qtls[, 2])

  # DECOMMENT WHEN TESTING, MUTE WHEN RUNNING
  # sapply(1:(ncol(cond.qtls)-1), function(i) {
  #   sapply((i+1):ncol(cond.qtls), function(j) {
  #     print(c(i, j)) # inspect copula to select
  #     print(BiCopSelect(cond.pobs[, i], cond.pobs[, j], familyset=NA))
  #   })
  # }) # all pick t copula

  # sigma <- cor(cond.qtls) # sigma # z <- mvrnorm(n, mu = rep(0, 3), Sigma=sigma,empirical=T)
  tcop.coef <- tCopula(dim = 3) %>% fitCopula(cond.pobs, method='ml') %>% coef
  tcop <- tCopula(dim = 3, param = tcop.coef[1], df = tcop.coef[2]) # persp(tcop, dCopula) # dim 2
  tcop
})


GenShBool <- function(bullet, target) {
  shlist <- bullet %>% GenHC(target) %>% {GenBinShHC(.$merge)}
  bools <- bullet %>% GenBoolSignal(n.group = 9)
  lapply(1:length(shlist), function(i) bools[shlist[[i]]] %>%
           do.call(cbind, .) %>% apply(1, any) %>% xts::xts(as.Date(names(.))))
}

bools.ind.XLK <- GenShBool(bullets$XLK$XLK.mom.3m, ret$XLK %>% lag.xts(-1))
ddens.ind.XLK <- bools.ind.XLK %>% GenCondGroups(ret$XLK, .) %>%
  GenBKDE %>% {(.$dens[[1]] - .$dens[[2]])/2}
da.cm.XLK <- bools.cm %>% GenCondGroups(ret$XLK, .) %>% GenBKDE

bools.ind.XLY <- GenShBool(bullets$XLY$XLY.mom.3m, ret$XLY %>% lag.xts(-1))
ddens.ind.XLY <- bools.ind.XLY %>% GenCondGroups(ret$XLY, .) %>%
  GenBKDE %>% {(.$dens[[1]] - .$dens[[2]])/2}
da.cm.XLY <- bools.cm %>% GenCondGroups(ret$XLY, .) %>% GenBKDE %>% {.$dens}

bools.ind.XLF <- GenShBool(bullets$XLF$XLF.mom.3m, ret$XLF %>% lag.xts(-1))
ddens.ind.XLF <- bools.ind.XLF %>% GenCondGroups(ret$XLF, .) %>%
  GenBKDE %>% {(.$dens[[1]] - .$dens[[2]])/2}
da.cm.XLF <- bools.cm %>% GenCondGroups(ret$XLF, .) %>% GenBKDE

states <- list(bools.ind.XLK, bools.ind.XLB, bools.ind.XLF, bools.cm) %>%
  lapply(function(x) x[[2]]) %>% do.call(cbind, .) %>%
  `colnames<-`(c('XLK', 'XLB', 'XLF', 'common')) * 1 + 1


da.cm.XLF$axis[sum(cumsum(da.cm.XLF$dens[[1]]) < u)]

# list(bullets$SPY, bullets$XLF, bullets$XLI, bullets$XLK, bullets$XLV, bullets$XLY) %>% {do.call(cbind, .)}
# bools <- GenBoolSignal(bullets.spy$`spy.mom.3m`, 9)
# dens <- bools %>% {GenCondGroups(ret.xlk %>% lag.xts(-1), .)} %>% GenBKDE
# sh.dens <- dens$dens %>% GenBinShDens(shlist.spy.xlk$`spy.mom.3m`) # shrinked group densities

u <- rCopula(50000, tcop) # get copula simulations
# plot(u[, 1], u[, 3], pch='.', col='blue'); cor(u, method='spearman')
sim <- lapply(1:ncol(u), function(i){
  as.vector(sapply(u[, i], function(x) quantile(cond.rets[, i], x)))
  # TODO: use different adjusted conditional returns
}) %>% do.call(cbind, .)

par(mfrow = c(3, 1))
for (i in 1:3) {cond.rets[, i] %>% hist(breaks = 300)}
for (i in 1:3) {sim[, i] %>% hist(breaks = 300)}

write.table(sim, file = "./data-raw/sample.csv", col.names = F, row.names = FALSE, sep=',')
  # write.csv will include colnames
