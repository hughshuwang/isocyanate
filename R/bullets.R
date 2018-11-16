#' Mixing a pack of signal with a single benchmark signal (i.e. DRI)
#'
#' @param pack a list of signals for DM/EM both, 2 columns, properly named
#' @param bmk a signal for DM/EM, 2 columns, DRI in DRIVS
#' @param method string of method used to mix
#' @return a list of signals after mixing
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
MixBmkSignal <- function(pack, bmk, method = c('none', 'mean', 'sa', 'max')) {
  if (method == 'none') {mixed <- pack}
  else if (method == 'mean') {mixed <- lapply(pack, function(new) (new + bmk) / 2)}
  else if (method == 'sa') {mixed <- lapply(pack, function(new) sqrt((new^2 + bmk^2) / 2))}
  else if (method == 'max') {mixed <- lapply(pack, function(new) {xts(pmax(as.data.frame(bmk), as.data.frame(new)), index(bmk))})}
  mixed
}


#' Mix signals in 2 packs (DM and EM packs) and cbind
#'
#' @param pack.dm list of dm signals/xts series
#' @param pack.em list of em signals/xts series
#' @param method stirng of method used
#' @return a xts with 2 columns DM/EM, each column is a kind of
#'   mix of 2 packs
#' @importFrom magrittr %>%
#' @export
MixPackSignal <- function(pack.dm, pack.em, method = c('mean', 'sa', 'max')) {
  SqAvg <- function(series) {sqrt(mean(series^2))} # squared average

  if (method == 'mean') {FUN <- mean}
  else if (method == 'sa') {FUN <- SqAvg}
  else if (method == 'max') {FUN <- max}

  cbind(apply(pack.dm, 1, FUN), apply(pack.em, 1, FUN)) %>%
    `colnames<-`(c("DM", "EM")) %>% xts::xts(zoo::index(pack.dm))
}


#' Generate a list of signals, standard pack including cont and binary signals
#'
#' @param monthly.var xts object, key variables to generate 0-1 signals
#' @param var.name string all lower case
#' @param backtest.period string that can used for indexing
#' @param IF.LAG bool
#' @param rolling.period int num of periods to be applied in the rolling window
#' @return list of xts signal series with proper colnames
#' @description compared to previous version in the script, change ZS and EQ to monthly calculation,
#'   now have higher std and higher range, more sensitive, higher corr more than 80%
#' @importFrom magrittr %>%
#' @export
StdMonthlyPack <- function(monthly.var, var.name, backtest.period, IF.LAG, rolling.period = 36) {
  FUNs <- c('FS' = GenFeatScale, 'ZS' = GenZScore, 'EQ' = GenEmpQuantile, 'SP' = GenSpike,
            'IHMd' = IfHigherMedian, 'IHMn' = IfHigherMean, 'ISP' = IfSpike) # fixed
  signal.names <- lapply(names(FUNs), function(x) paste(var.name, x, sep = '.')) %>% unlist # what matter are names of the cols instead of vars
  lapply(seq_along(FUNs), function(x) SignalWrapper(GenRelScore, list(monthly.var, 'monthly', rolling.period, 0, FUNs[[x]], NULL, 'monthly'),
    backtest.period, signal.names[x], IF.LAG)) %>% `names<-`(names(FUNs)) # generate signals inside
}


#' Generate a list of signals, binary pack with if higher than mean, median and quantiles
#'
#' @param monthly.var xts object, key variables to generate 0-1 signals
#' @param var.name string all lower case
#' @param backtest.period string that can used for indexing
#' @param IF.LAG bool
#' @param rolling.period int num of periods to be applied in the rolling window
#' @return list of xts signal series with proper colnames
#' @importFrom magrittr %>%
#' @importFrom purrr partial
#' @export
StdBinaryPack <- function(monthly.var, var.name, backtest.period, IF.LAG, rolling.period = 36) {
  FUNs <- c('Q20' = partial(IfHigherQuantile, q=0.2), 'Q30' = partial(IfHigherQuantile, q=0.3),
            'Q40' = partial(IfHigherQuantile, q=0.4), 'Q50' = partial(IfHigherQuantile, q=0.5),
            'Q60' = partial(IfHigherQuantile, q=0.6), 'Q70' = partial(IfHigherQuantile, q=0.7),
            'Q80' = partial(IfHigherQuantile, q=0.8), 'IHMn' = IfHigherMean)
  signal.names <- lapply(names(FUNs), function(x) paste(var.name, x, sep = '.')) %>% unlist
  lapply(seq_along(FUNs), function(x) SignalWrapper(GenRelScore, list(monthly.var, 'monthly', rolling.period, 0, FUNs[[x]], NULL, 'monthly'),
    backtest.period, signal.names[x], IF.LAG)) %>% `names<-`(names(FUNs))
}
