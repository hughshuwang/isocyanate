# STANDARD EDE-BASED ALPHA SIGNAL RESEARCH PROCEDURE #

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
load("data/sectors.rda")
logret <- log(sec.varset$changep + 1)
 1
regres <- logret %>% RollingRegres(21, 'changep.SPY')
regres %>% cor %>% round(2) # res not correlated
pca1 <- prcomp(regres[, 1:9], scale. = TRUE)

rollalpha <- logret %>% RollingAlpha(21, 'changep.SPY')
rollalpha %>% acf

  # positive xlb xli xle, material, industrials, and energy
  # negative xlp & xlk, technology and consumer staples
  # TODO: negative on lag explanation?

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
