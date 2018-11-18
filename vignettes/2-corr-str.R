# CORR MODELING AND COPULA SIMULATIONS #

rm(list = ls())

library(isocyanate)
library(magrittr)
library(xts)
library(zoo)

set.seed(12896)
load("../data/sectors.rda")
logret <- log(sec.varset$changep + 1)

# TODO: output cluster information
GenHCList <- function(hc.merge, n.group) {
    hc.merge <- hc$merge; n.group <- 4

    clusters <- vector()
    n <- nrow(hc.merge)
    crow <- hc.merge[n, ]

    append(clusters, -crow[crow < 0])
}

# TODO: Gen cuts for individual assets' common bullets
# TODO: Merge cuts
# TODO: Estimate conditional corr copula structure
# TODO: Simulate using conditional corr and overall empirical CDF
# TODO: export sample
