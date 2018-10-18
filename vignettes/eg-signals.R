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

# load("data/factors.rda")
load("data/sectors.rda")
logret <- log(sec.varset$changep + 1)

# Inspect alpha: estimate corr effect ----
winlen <- 50
alphas <- rollapply(logret, winlen, function(df) {
  apply(df, 2, function(vec, mkt) {
    vec[length(vec)] - lm(vec ~ mkt)$coefficients[2] * mkt[length(mkt)]
  }, mkt = df[,ncol(df)])
}, by.column = F) %>% na.omit
alphas %>% cor %>% round(2)
pca1 = prcomp(alphas[, 1:9], scale. = TRUE)

  # CONCLUSION:
  # for 6 factors: alpha corr around 20, highest 30, some low
  # for 9 sectors: alpha corr on average lower than 10%,
  # so the bias generated from inaccurate conditional corr
  # will not be very high

  # DRAFT OF CONDITIONAL ALPHA FOR FACTORS:
  # good.idx <- ret$changepSPY > -0.001
  # good.idx <- good.idx[zoo::index(alphas)]
  # par(mfrow = c(3, 2))
  # for (i in 1:6) {
  #   hist(alphas[good.idx, i], breaks = 200, main = colnames(alphas)[i], xlim = c(-0.02, 0.02))
  #   # for each one, do a smooth density and plot it in the plot
  #   # look at the distribution and determine the properties
  #   # monitor the sample size
  # }
  # for (i in 1:6) {
  #   hist(alphas[!good.idx, i], breaks = 200, main = colnames(alphas)[i], xlim = c(-0.02, 0.02))
  # } # USMV very difference # when market down, size higher risk, mv and vlue lower risk


# Stage I: EDE-based Signal Selection ----

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

# TECHNOLOGY SECTOR
ret.xlk <- logret$changep.XLK # 1, (2, 3, 4, 10), (5, 6, 7, 8, 9)
dens.bounds <- c(quantile(ret.xlk, 0.001), quantile(ret.xlk, 1-0.001))
ret.xlk <- ret.xlk[ret.xlk < dens.bounds[2] & ret.xlk > dens.bounds[1]]
groups <- lapply(bools, function(bool) {xts::merge.xts(ret.xlk, bool) %>% na.omit() %>%
    `colnames<-`(c('ret', 'idx')) %>% {.$ret[.$idx != 0]}})

bw <- 0.003; gs <- 128L # TODO: how to tune this? adaptive smoothing
dens <- lapply(groups, function(group) {
  # ash1(bin1(sample, ab = dens.bounds, nbin = 128))$y %>% {./sum(.)} # EDE1
  bkde(group, "normal", FALSE, bw, gs, dens.bounds)$y %>% {./sum(.)} # EDE2
}) # densities

bbands <- lapply(groups, function(group) {
  subs <- do.call(rbind, lapply(1:100, function(i) {
    sub.idx <- sample(1:length(group), length(group) * pctg)
    bkde(group[sub.idx], "normal", bandwidth = bw, gridsize = gs, range.x = dens.bounds)$y %>% {./sum(.)}
  }))
  means <- subs %>% apply(2, mean); sds <- subs %>% apply(2, stats::sd)
  cbind('2sd lower' = means - 2*sds, 'mean' = means, '2sd upper' = means + 2*sds)
  # plot(means, type = 'l'); lines(means - 2*sds, col = "blue"); lines(means + 2*sds, col = "red")
})
bbmean <- lapply(bbands, function(df) df[, 2]) # bb mean
bbnoise <- lapply(bbands, function(df) df[, 2] - df[, 1]) # bb std

sumsq <- function(dens, i, j) {sum((dens[[i]] - dens[[j]]) ^ 2)} # distance func
adjsq <- function(){} # adjusted measure for distance, give more weights

dmat <- sapply(1:n.group, function(i) {
  sapply(1:n.group, function(j) {
    sumsq(dens, i, j)
  })
}) # distance matrix

hc <- hclust(as.dist(dmat), method = "average") # different method yield different results
plot(hc); hc$height # if n.group large (20), height increase, but some from sampling noise
# pretty much the same results for ASH and KernSmooth
# decision making for cutting conditions: eyeballing for now

hc$order # spotify high/low volatility situation

  # estimate density of sample, and formulate the right output
  # TODO: plot outputs to show results, pick different time period to TEST CONSISTENCY
  # TODO: define new measure that can give tail more weights
  # TODO: formulate an out-of-sample test of fitting the distribution compared with

