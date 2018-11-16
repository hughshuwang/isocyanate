#' Decompose the huge metrics dataframe into a list
#'
#' @param metrics dataframe from OutputWrapper
#' @return a list of dataframe, each df for one variable
#'   the names of the list are variable names, in each df
#'   colnames are adjusted to be just names of methods,
#'   to save space
#' @importFrom magrittr %>%
#' @export
DecompMetrics <- function(metrics) {
  # decompose colnames into a list of vectors of strings
  decomp.names <- lapply(colnames(metrics[, -1]), SplitStratName) # defined below
  var.names <- decomp.names %>% lapply(function(x) x[1]) %>% unlist %>% unique

  GenSubMetrics <- function(name) { # mind the global vars in the upper level env
    match.idx <- sapply(decomp.names, function(x) x[1] == name)

    # build output dataframe and new colnames (without variable names to be shorter)
    output <- cbind(metrics[, 1, drop = FALSE], metrics[, -1][, match.idx])
    new.colnames <- decomp.names[match.idx] %>% lapply(function(x) x[2]) %>% unlist

    `colnames<-`(output, c(colnames(metrics)[1], new.colnames))
  } # get one dataframe for one name

  `names<-`(lapply(var.names, GenSubMetrics), var.names)
}


#' Split the strategy name into a vector of 2
#'
#' @param strategy.name string of variable.METHOD,
#'   method name (intials) should be all captitalized,
#'   variable name can have '.' but NO uppercase letter
#' @return a vector of 2 strings, variable and scoring method
#' @import stringr
#' @export
SplitStratName <- function(strategy.name) {
  # use the first uppercase letter to cut
  # first part variable name, second part method name
  cut <- str_locate_all(strategy.name, pattern = "[:upper:]")[[1]][1, 1]
  # function from stringr, locate all matches

  scoring.method <- substring(strategy.name, cut)
  variable <- substring(strategy.name, 1, cut-2)

  c(variable, scoring.method)
}


#' Generate metrics dataframe for permutation resampling
#'
#' @param signal.bmk single xts object, signal with DE rep
#' @param n int number of resampling
#' @param inits initial weights of the portfolio, in US, DM rep
#' @param asset.ret xts object of returns of assets traded in the portfolio
#' @param name.bmk string the name for the strategy we're interested in
#' @param signal.altbmk xts object alternative benchmark signal for getting relative perf
#' @return metric dataframe
#' @importFrom magrittr %>%
#' @examples
#'   metrics.random <- PermMetrics(signal.hybrid, 5000, c(0, 0.34/0.49), asset.ret, 'hybrid')
#' @export
PermMetrics <- function(signal.bmk, n, inits, asset.ret, name.bmk, signal.altbmk) {
  random.signals <- c(list(signal.bmk), lapply(1:n, function(i) { # bmk put as the first one
    cbind(sample(as.vector(signal.bmk[, 1]), dim(signal.bmk)[1]),
          sample(as.vector(signal.bmk[, 2]), dim(signal.bmk)[1])) %>%
      xts::xts(zoo::index(signal.bmk)) %>% `colnames<-`(c('DM', 'EM'))
  }))

  alt.ws <- GenWeights(list(signal.altbmk), 'DE', inits, asset.ret); alt.trets <- GenTrets(alt.ws, asset.ret, "Monthly")[[1]]
  weights <- GenWeights(random.signals, 'DE', inits, asset.ret); trets <- GenTrets(weights, asset.ret, "Monthly")

  GenOutputs(weights, trets, asset.ret, "Monthly", c(name.bmk, as.character(1:n)), alt.trets)[[1]] # report the huge metrix
}


#' Generate a list of weight dataframes for a list of signals
#'
#' @param signal.pack list of signals with columns properly named, the first one
#'   will be taken as the benchmark signal, each signal should be a xts object with
#'   one or multiple columns, signal representation should match param signal.rep
#' @param signal.rep string indicating signal representation, 'UD' means 2 series,
#'   US signal and DM signal, 'DE' means 2 series, DM signal and EM signal (old DRI)
#' @param inits vector of numerics, representing new weights c(us.init, dm.init)
#' @param returns a xts dataframe with columns being assets traded in the portfolio, should
#'   have the same number of columns as member in weights
#' @return a list of weight dataframes/xts objects, with number of columns same as number of
#'   columns in returns (returns of assets traded in the portfolio)
#' @export
GenWeights <- function(signal.pack, signal.rep = c('UD', 'DE'), inits = NULL, returns) {
  # pick weights generating function
  if (signal.rep == 'UD') {GenFunc <- GenWsUD} else if (signal.rep == 'DE') {GenFunc <- GenWsDE}
  signal.pack %>% lapply(GenFunc, names = colnames(returns), us.init = inits[1], dm.init = inits[2])
}


#' Generate a list of total returns of the portfolio guided by the signal
#'
#' @param weights a list of xts dataframes, representing weights time series, generated
#'   from GenWeights, with names properly set as signal.names
#' @param returns a xts dataframe with columns being assets traded in the portfolio, should
#'   have the same number of columns as member in weights
#' @param returns.period string, indicating periodicty of returns, can be used to calculate daily
#'   performance metrics if passed a daily return dataframe and monthly weights
#' @param sub.period vector of xts time index, can be indexed by xts objects, default NULL
#'   can only take monthly sub.period index for now, when the returns are daily, only get
#'   the first day of each month, raise error
#' @return a list of xts series, total returns of signals, with names properly set as signal.names
#' @description Generalized version for daily and monthly data, for fragmental time periods
#' @export
GenTrets <- function(weights, # obtained by GenWeights
                     returns, # asset.returns subsetted from monthly.ret
                     returns.period = c("Monthly", "Daily"),
                     sub.period = NULL) {
  n.assets <- ncol(returns)
  lapply(weights, function(weight) { # just return the trets list
    # left join to get time index of returns
    combined <- xts::merge.xts(returns, weight, join = 'left')
    combined[1, (n.assets + 1):ncol(combined)] <- weight[1, 1:n.assets]
    # fill the first day, if it's not the first day in a month

    combined <- zoo::na.locf(combined) # back fill
    tret <- combined[, (n.assets + 1):ncol(combined)] * combined[, 1:n.assets]

    if (!is.null(sub.period)) {
      if (returns.period == 'Monthly') {tret <- tret[sub.period]}
      else {stop("Sub period can only be used for monthly data for now")}
    }
    apply(tret, 1, sum) # this process will work for both daily and monthly data
  }) # list of returns for full time period specified in returns dataframe
}


#' Generate complete outputs for a list of signals, weights, and trets
#'
#' @param weights a list of xts dataframes, representing weights time series, generated
#'   from GenWeights, with names properly set as signal.names
#' @param trets a list of xts series, total returns of signals, generated from GenTrets,
#'   also with names properly set as signal.names
#' @param returns a xts dataframe with columns being assets traded in the portfolio, should
#'   have the same number of columns as member in weights
#' @param returns.period string, indicating periodicty of returns, can be used to calculate daily
#'   performance metrics if passed a daily return dataframe and monthly weights
#' @param signal.names should be passed from outside environment, a vector of string, previously
#'   set as unlist(lapply(signal.pack, function(x) colnames(x)[1])) being default
#' @return a list of metrics dataframe and pnl xts object with proper colnames
#' @importFrom magrittr %>%
#' @export
GenOutputs <- function(weights, trets, # lists
                       returns, returns.period = c("Monthly", "Daily"),
                       signal.names, bmk.ret = trets[[1]]) {
  raw.outputs <- lapply(trets, GenCoreMetrics, benchmark.ret = bmk.ret, period = returns.period) # pnl and metrics
  twoway.to <- weights %>% lapply(GenTurnover, returns = returns) %>% unlist # turnovers
  avgw.us <- weights %>% lapply(function(df) colMeans(df)['SPY']) %>% unlist # average SPY weights

  metrics <- do.call(cbind, lapply(raw.outputs, function(l) l[[1]])) %>% rbind(twoway.to * 100, avgw.us * 100)
  n <- nrow(metrics); rownames(metrics)[c(n-1, n)] <- c("Avg Turnover", "Avg SPY Alloc")

  pnls <- do.call(cbind, lapply(raw.outputs, function(l)l[[2]]))

  list(`colnames<-`(metrics, signal.names), `colnames<-`(pnls, signal.names))
}


#' Get core performance metrics for monthly log returns
#'
#' @param total.ret xts object monthly log returns
#' @param banchmark.ret xts same size and index of total.ret
#' @param market.ret xts same size and index of total.ret
#' @param peroid period of returns, for diff annualized factors
#' @param rf numeric monthly risk free rate, default: 3\%
#' @export
GenCoreMetrics <- function(total.ret, benchmark.ret = total.ret,
                           period = c("Monthly", "Daily"),
                           market.ret = NULL, rf = 0) {
  if (period == "Monthly") {ann.factor <- 12}
  else if (period == "Daily") {ann.factor <- 12 * 21}

  m <- mean(total.ret) * ann.factor
  sd <- stats::sd(total.ret) * sqrt(ann.factor)
  dn.dev <- sqrt(mean((total.ret[total.ret < 0] ^ 2))) * sqrt(ann.factor) # sortino sd
  sharpe <- (m - rf) / sd
  sortino <- (m - rf) / dn.dev
  positive.pctg <- mean(total.ret > 0)

  worst.ret <- min(total.ret)
  which.worst <- zoo::index(total.ret)[which.min(total.ret)]

  pnl <- exp(cumsum(total.ret))
  maxDD <- max(1 - pnl / sapply(seq_along(pnl), function(x) max(pnl[1:x])))
  corr <- cor(total.ret, benchmark.ret)

  active.ret <- total.ret - benchmark.ret
  active.m <- mean(active.ret) * ann.factor
  active.sd <- stats::sd(active.ret) * sqrt(ann.factor) # also TE
  information.ratio <- (active.m - rf) / active.sd
  active.positive.pctg <- mean(active.ret > 0)
  active.worst.ret <- min(active.ret)
  active.which.worst <- zoo::index(active.ret)[which.min(active.ret)]

  pnl.active <- exp(cumsum(active.ret))
  active.maxDD <- max(1 - pnl.active / cummax(pnl.active)) # 2nd way to cal

  up.index <- benchmark.ret > 0 # use benchmark.ret to seperate 2 groups of months
  up.cap <- mean(total.ret[up.index]) / mean(benchmark.ret[up.index])
  down.cap <- mean(total.ret[!up.index]) / mean(benchmark.ret[!up.index])

  metrics <- c('Mean' = m * 100, 'Stdev' = sd * 100, 'Downside Dev' = dn.dev * 100,
               'Sharpe' = sharpe, 'Sortino' = sortino, 'Obj' = (m - sd/2) * 100,
               '% of Positive' = positive.pctg * 100,
               'MaxDD' = maxDD * 100, 'Worst' = worst.ret * 100,
               'Mean vs BMK' = active.m * 100, 'TE vs BMK' = active.sd * 100,
               'IR' = information.ratio,
               'Up Capture' = up.cap * 100, 'Down Capture' = down.cap * 100,
               '% >= BMK' = active.positive.pctg * 100,
               'MaxDD vs BMK' = active.maxDD * 100, 'Worst vs BMK' = active.worst.ret * 100)
  list(metrics, pnl)
}


#' Generate two way turnover
#'
#' @param bop.weight single xts object of Beginning of Period Weights,
#'   each column represents one assets
#' @param returns xts object of log returns, same periodicity and size
#'   as bop.weight
#' @return average monthly two-way turnover rate, sum long and short changes
#'   in weights when rebalancing annualized
#' @importFrom magrittr %>%
#' @export
GenTurnover <- function(bop.weight, returns) {
  eop.weight <- (bop.weight * exp(returns)) %>% {./rowSums(.)}
  stopifnot(all(abs(rowSums(eop.weight) - 1) < 0.0000001))
  (bop.weight - xts::lag.xts(eop.weight)) %>% na.omit %>% abs %>% rowSums %>% mean * 12
}


#' Generate global weights for US/Intl(DM/EM) (UD rep)
#'
#' @param UD.signal xts object with 2 columns (us.signal and dm.signal).
#'   us.signal: xts object series of signals of US market in the overall
#'   portfolio, ranging from 0 to 1, default: monthly periodicity.
#'   dm.signal xts objects series of signals of DM in the international
#'   portfolio, default: monthly periodicity, ranging from 0 to 1, calculated
#'   in the same way as us.signal check 2 signals have the same index as monthly return
#' @param names colnames of output, user should double check
#' @param us.init,dm.init numeric initial weight of us market in overall, dm in intl
#' @return xts object series of weights of US, DM, EM in
#'   the international portfolio, using same index as input signals
#' @description designed for the following scenario: US/Intl(DM/EM),
#'   preset initial (index) weights in portfolio presented by US weight in
#'   overall portfolio and DM weight in international portfolio, pass in
#'   signal series with value ranging from 0 to 1, with the same index as
#'   monthly return
#' @export
GenWsUD <- function(UD.signal, names, us.init = 0.5, dm.init = 0.34/0.15) {
  us.signal <- UD.signal[, 1]; dm.signal <- UD.signal[, 2]

  us.ws <- us.signal * (1 - us.init) + us.init # between init.US and 1
  dm.ws <- dm.signal * (1 - dm.init) + dm.init # with the same index as us.ws
  intl.ws <- cbind(dm.ws, 1 - dm.ws) # weights in intl portfolio
  global.ws <- cbind(us.ws, apply(intl.ws, 2, function(x) x * as.vector(1 - us.ws)))

  stopifnot(all(apply(global.ws, 1, sum) - 1 < 0.0000001))
  `colnames<-`(global.ws, names) # the weights series has the same index as signal inputs
}


#' Generate global weigths for US/Intl(DM/EM) using original DRI (DE rep)
#'
#' @param DE.signal xts object of DRI signal (DE rep, trans to dri.dm, dri.em),
#'   representing proportion of initial weights in global portfolio in DM (EFA)
#'   and EM (EEM) that should be transfer to the US portfolio (original DRI signal)
#' @param names colnames of output, i.e. colnames(monthly.ret)
#' @param us.init,dm.init numeric initial weights of us in global, dm in intl
#' @export
GenWsDE <- function(DE.signal, names, us.init = 0.5, dm.init = 0.34/0.15) {
  dm <- DE.signal[, 1]; em <- DE.signal[, 2]

  us.ws <- us.init + dm * (1 - us.init) * dm.init + em * (1 - us.init) * (1 - dm.init)
  dm.ws <- (1 - dm) * (1 - us.init) * dm.init
  em.ws <- (1 - em) * (1 - us.init) * (1 - dm.init)
  global.ws <- cbind(us.ws, dm.ws, em.ws)

  stopifnot(all(apply(global.ws, 1, sum) - 1 < 0.0000001))
  `colnames<-`(global.ws, names)
}

