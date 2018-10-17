#### THIS SCRIPT IS UNDER CONSTRUCTION ####

rm(list = ls())

load("data/factors.rda")
load("data/sectors.rda")

ret <- sec.vars$changep
alphas <- rollapply(ret, 50, function(df) {
  apply(df, 2, function(vec, mkt) {
    vec[length(vec)] - lm(vec ~ mkt)$coefficients[2] * mkt[length(mkt)]
  }, mkt = df[,ncol(df)])
}, by.column = F) %>% na.omit

alphas %>% cor %>% round(2)
#             changepMTUM changepQUAL changepVLUE changepSIZE changepUSMV
# changepMTUM        1.00        0.11       -0.29        0.01        0.21
# changepQUAL        0.11        1.00       -0.08        0.06        0.29
# changepVLUE       -0.29       -0.08        1.00        0.29       -0.25
# changepSIZE        0.01        0.06        0.29        1.00        0.17
# changepUSMV        0.21        0.29       -0.25        0.17        1.00


# build a tool, for individual conditions or overall conditions
# ind: one return, one TF series
# ovr: one dataframe, one TF series

good.idx <- ret$changepSPY > -0.001
good.idx <- good.idx[zoo::index(alphas)]

par(mfrow = c(3, 2))
for (i in 1:6) {
  hist(alphas[good.idx, i], breaks = 200, main = colnames(alphas)[i], xlim = c(-0.02, 0.02))
  # for each one, do a smooth density and plot it in the plot
  # look at the distribution and determine the properties
  # monitor the sample size
}
for (i in 1:6) {
  hist(alphas[!good.idx, i], breaks = 200, main = colnames(alphas)[i], xlim = c(-0.02, 0.02))
} # USMV very difference # when market down, size higher risk, mv and vlue lower risk


vars.xts$rollmean <- vars.xts$changep %>% rollapply(21, mean) %>% na.omit
vars.xts$rollvcov <- vars.xts$changep %>% GenVCOV('daily', VecVCOV, 21)

vars.tbl <- vars.xts %>% map(tibbleXTS)

map(seq_along(vars.tbl), function(i) write.csv(vars.tbl[[i]], file.names[i], row.names=FALSE))

save(vars.tbl, file = "data/Pop_variables.rda")



do.call(cbind, lapply(var.list, function(df) df[, 'changep'])) %>% cor %>% round(3)
# TODO: test the corr across rolling alphas of factors and sectors

# Return Quantile Conditional Tests ====
ret <- daily.ret[, "SPY"]
hist(ret, breaks = 500, freq = FALSE) # TODO: cut the ret
curve(dnorm(x, mean=mean(ret), sd=sd(ret)), add=TRUE)

winlen <- 5 # fix a week
quantiles <- ret %>% zoo::rollapply(winlen, sum) %>% na.omit %>% as.vector %>% sapply(., stats::ecdf(.)) %>%
  xts::xts(order.by = zoo::index(ret)[winlen:length(ret)])

n.group <- 6 # fix
# cuts <- seq(0, 1, 1/n.group) # equally spaced, quantile cuts, make sure subgroup size and avoid cutting line
cuts <- c(0, 1/10, 3/10, 0.5, 1-3/10, 1-1/10, 1)
groups <- lapply(1:n.group, function(i) {
  (quantiles < cuts[i+1] & quantiles > cuts[i]) %>% xts::xts(zoo::index(quantiles)) %>%
    xts::lag.xts(1) %>% xts::merge.xts(., ret) %>% na.omit() %>%
    `colnames<-`(c('idx', 'ret')) %>% {.$ret[.$idx != 0]}
})

par(mfrow = c(2, 3))
# TODO: cut before drawing plots, so that they have same break indexes
hists <- lapply(1:n.group, function(x) {
  sub.ret <- groups[[x]]
  hist(sub.ret, breaks = 200, xlim = c(-0.1, 0.1), freq = FALSE)
  curve(dnorm(x, mean = mean(sub.ret), sd = sd(sub.ret)), add = T)
})

lapply(1:n.group, function(x) {
  hists[[x]]$density %>% plot(type = 'l')
})

lapply(2:n.group, function(x) {
  (hists[[1]]$density - hists[[x]]$density) %>% plot(type = 'l')
})

# Stage II: conditioning on systematic indicators, capula sampling from estimated density
# TODO: time index mgmt

