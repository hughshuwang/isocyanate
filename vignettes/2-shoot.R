# CORR MODELING, COPULA SIMULATIONS, AND DCP #

rm(list = ls())

library(ash)
library(copula)
library(CVXR)
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

asset.tickers <- c('XLK', 'XLY', 'XLF')

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
  cond.rets <- ret[, asset.tickers] %>%
    lapply(GenCondGroups, bools=bools.cm) %>%
    lapply(function(x) x[[i]]) %>% do.call(cbind, .) %>%
    `colnames<-`(asset.tickers)
  cond.qtls <- lapply(1:ncol(cond.rets), function(i)
    GenEmpQuantileVec(cond.rets[, i])) %>%
    Reduce(xts::merge.xts, .) %>% `colnames<-`(asset.tickers) # conditional quantiles
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

# TODO: pack up in shoot.R
GenShBool <- function(bullet, target) {
  shlist <- bullet %>% GenHC(target) %>% {GenBinShHC(.$merge)}
  bools <- bullet %>% GenBoolSignal(n.group = 9)
  lapply(1:length(shlist), function(i) bools[shlist[[i]]] %>%
           do.call(cbind, .) %>% apply(1, any) %>% xts::xts(as.Date(names(.))))
}

bools.ind.XLK <- GenShBool(bullets$XLK$XLK.mom.3m, ret$XLK %>% lag.xts(-1))
ddens.ind.XLK <- bools.ind.XLK %>% GenCondGroups(ret$XLK, .) %>%
  GenBKDE %>% {(.$dens[[1]] - .$dens[[2]])/2} %>% {list(., 0 * .)}
da.cm.XLK <- bools.cm %>% GenCondGroups(ret$XLK, .) %>% GenBKDE

bools.ind.XLY <- GenShBool(bullets$XLY$XLY.mom.3m, ret$XLY %>% lag.xts(-1))
ddens.ind.XLY <- bools.ind.XLY %>% GenCondGroups(ret$XLY, .) %>%
  GenBKDE %>% {(.$dens[[1]] - .$dens[[2]])/2} %>% {list(., 0 * .)}
da.cm.XLY <- bools.cm %>% GenCondGroups(ret$XLY, .) %>% GenBKDE

bools.ind.XLF <- GenShBool(bullets$XLF$XLF.mom.3m, ret$XLF %>% lag.xts(-1))
ddens.ind.XLF <- bools.ind.XLF %>% GenCondGroups(ret$XLF, .) %>%
  GenBKDE %>% {(.$dens[[1]] - .$dens[[2]])/2} %>% {list(., 0 * .)}
da.cm.XLF <- bools.cm %>% GenCondGroups(ret$XLF, .) %>% GenBKDE

states <- list(bools.ind.XLK, bools.ind.XLY, bools.ind.XLF, bools.cm) %>%
  lapply(function(x) x[[2]]) %>% do.call(cbind, .) %>%
  `colnames<-`(c(asset.tickers, 'common')) * 1 + 1

lapply(1:3, function(i) {
  day <- index(states)[i]
  tcop <- tcops[[states[day, 'common']]]
  u <- rCopula(5000, tcop)
  das.cm <- list(da.cm.XLK, da.cm.XLY, da.cm.XLF)
  ddens <- list(ddens.ind.XLK, ddens.ind.XLY, ddens.ind.XLF)

  sims <- lapply(1:ncol(u), function(n) {
    dens.cm <- das.cm[[n]]$dens[[states[day, 'common']]]
    dens.adj <- ddens[[n]][[states[day, 'XLK']]]
    dens <- (dens.cm + dens.adj) %>% {./sum(.)}
    axis <- das.cm[[n]]$axis
    sapply(u[, 1], function(i) {
      sum(cumsum(dens) < i) %>% {min(axis)+(max(axis)-min(axis))/length(axis)*.}
    })
  }) %>% do.call(cbind, .) %>% `colnames<-`(asset.tickers)

  mu <- colMeans(sims); sigma <- cov(sims)
  dsigma <- cov(sims[apply(sims < 0, 1, all), ])

  w0 <- c(0,0,0,1) # plus cash, full cash first
  z <- Variable(ncol(sims)+1)

  obj <- - t(w0+z)[1:3] %*% mu + 0.5 * quad_form((w0+z)[1:3], dsigma) # convex
  netout <- sum_entries(z)
  consts <- list(w0+z >= 0, w0+z <= 1, netout == 0)
  prob <- Problem(Minimize(obj), consts)
  result <- tryCatch({solve(prob)}, error = function(e) {print(e)})
  t(result$getValue(z) %>% round(3))
}) %>% {do.call(rbind, .)}

# TODO: wrapup as a solver function
