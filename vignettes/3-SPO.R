rm(list = ls())

library(CVXR)
library(dplyr)
library(readr)
library(magrittr)

sample <- read_csv(file = "data-raw/sample.csv", col_names = F)
# with just simulated asset returns
mu <- colMeans(sample); sigma <- cov(sample)
dsigma <- cov(sample[apply(sample < 0, 1, all), ])

w0 <- c(0,0,0,1) # plus cash, full cash first
z <- Variable(ncol(sample)+1)

obj <- - t(w0+z)[1:3] %*% mu + 0.5 * quad_form((w0+z)[1:3], dsigma) # convex
netout <- sum_entries(z) + abs(z) * 0.001
consts <- list(w0+z >= 0, w0+z <= 1, netout <= 0)
prob <- Problem(Minimize(obj), consts)
result <- tryCatch({solve(prob)}, error = function(e) {print(e)})
result$getValue(z) %>% round(3)
