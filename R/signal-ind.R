#' Wrap up a general signal
#'
#' @param FUN function used for generating score
#' @param params list of params for FUN
#' @param backtest.period string used for indexing xts
#' @param name name of the signal column
#' @param if.lag bool if lag the signal to the next period
#' @return xts signal series object
#' @description
#' signal.cov.FS2 <- SignalWrapper(DRIVS::GenRelScore,
#'     list(monthly.vcov[, 'SPY & EE'], 'monthly', 36, 0,
#'     DRIVS::GenFeatScale, 'monthly'),
#' backtest.period, 'US', TRUE)
#'
#' signal.cov.FS <- stats::na.omit(xts::lag.xts(
#'     DRIVS::GenRelScore(monthly.vcov[, 'SPY & EE'], 'monthly',
#'                        rolling.period = 36,
#'                        burn.in.period = 0,
#'                        scoring.method = DRIVS::GenFeatScale,
#'                        output.period = 'monthly')
#' ))[backtest.period]; names(signal.cov.FS) <- "cov.FS"
#'
#' all(signal.cov.FS - signal.cov.FS2 == 0) # TRUE
#' @importFrom magrittr %>%
#' @export
SignalWrapper <- function(FUN, params,
                          backtest.period,
                          name = 'US',
                          if.lag = TRUE) {
  if (if.lag) {LAG.FUN <- xts::lag.xts} else {LAG.FUN <- xts::xts}
  FUN %>% do.call(params) %>% LAG.FUN() %>% stats::na.omit() %>% `[`(backtest.period) %>% `names<-`(name)
}



#' Generate dynamic relative scores for input matrix
#'
#' @param data xts object vectorized named rolling (vcov) dataframe
#' @param data.period string 'monthly' or 'daily', should match the data
#' @param rolling.period numeric number of data.period should be included
#'   in each calculation in the rolling window
#' @param burn.in.period numeric number of period (specified in data.period)
#'   that's used to generate the first realized value, can be included
#'   in the rolling period.
#' @param scoring.method function getting df and returning a vec
#' @param transfer.method function used to transfer daily result to monthly
#'   when output.period != data.period, from a vec to a value
#' @param output.period string 'monthly' or 'daily', frequency of return
#'   xts object, default: 'monthly'
#' @return xts object with monthly
#' @importFrom magrittr %>%
#' @export
GenRelScore <- function(data, data.period,
                        rolling.period,
                        burn.in.period = NULL,
                        scoring.method,
                        transfer.method = NULL,
                        output.period = 'monthly') {
  # generate xts rollapplied object without NA

  if (data.period == 'daily') {
    rolling.period <- rolling.period - burn.in.period
    # adjust the period for daily input, extract the burn.in.period
    # to make sure the rolling.period coverd in rollapply is correct
  }

  # core function rollapply
  score <- data %>% zoo::rollapply(rolling.period, scoring.method) %>% stats::na.omit()

  # wrap up outputs
  if (data.period != output.period) {
    # save monthly index first, as it will be matrix later
    monthly.index <- unique(zoo::as.yearmon(zoo::index(score)))

    # call transfer method and change index
    # apply the transfer.method on each column of the df for each month
    score <- score %>% xts::split.xts('months') %>%
      lapply(function(x) apply(x, 2, transfer.method)) %>%
      do.call(rbind, .) %>% xts::xts(order.by = monthly.index)
  }

  score
}



#' Generate sub period index based on robust rolling std range (filtering/regime id)
#'
#' @param series xts object, benchmark time series, monthly for now
#' @param winlen numeric, rolling window length for calculating sd and mean
#' @param sd.threshold integer, used for the boundary of standard deviation for the range
#' @param smooth.winlen integer, showing how many true in a row to get a true in the index,
#'   used for cleaning fragmental: minimum allowed len for index to be positive
#' @return zoo::index, monthly for now, can used by series[idx]
#' @description excluding values that are out of sd range (mean p/n threshold times sd)
#'   from the rolling window to get more stable range of changes
#'
#' @examples
#' # to filter the efa's EFA
#' pcg.SEE <- pe.SEE / xts::lag.xts(pe.SEE) - 1 # pe.SEE is a PE ratio xts dataframe
#' stable.idx.efa <- pcg.SEE$EFA %>% na.omit %>%
#'   GenSubidxSD(sd.threshold = 2, IF.LAG = TRUE, smooth.winlen = 3, winlen = 36)
#' pcg.SEE$EFA %>% na.omit %>%
#'   PlotFilterCompLine(stable.idx.efa, "Std Filter for EFA")
#'
#' @importFrom magrittr %>%
#' @importFrom zoo rollapply
#' @importFrom zoo index
#' @importFrom xts xts
#' @export
GenSubidxSD <- function(series,
                        winlen = 36,
                        sd.threshold = 3,
                        smooth.winlen = 3,
                        IF.LAG = FALSE) {
  stable.idx <- as.logical(c(rep(1, winlen), rep(0, (length(series) - winlen)))) # stable window for sd
  # note that the last one of the window is not used for calculateing sds

  # main process of applying moving window
  for (i in (winlen+1):length(series)) { # update starts at winlen + 1
    roll.mean <- series[stable.idx] %>% tail(winlen) %>% mean()
    roll.sd <- series[stable.idx] %>% tail(winlen) %>% stats::sd()

    if (series[i] <= roll.mean + sd.threshold * roll.sd
        && series[i] >= roll.mean - sd.threshold * roll.sd) {stable.idx[i] <- TRUE}
    # flip the indicator, add one, no need to update the winstart, get index from 1
  }

  # smoothing process for index with 3 periods in a row being positive
  smoothed.stable.idx <- c(rep(TRUE, smooth.winlen-1), rollapply(stable.idx, smooth.winlen, all)) %>% xts(index(series))
  stopifnot(length(smoothed.stable.idx) == length(stable.idx))

  # lag to get rid of future function, the pe ratio is observed at the end of the period
  if (IF.LAG) {idx <- c(TRUE, smoothed.stable.idx %>% xts::lag.xts() %>% stats::na.omit() %>% as.vector())}
  else {idx <- as.vector(smoothed.stable.idx)}

  stopifnot(length(idx) == length(series)) # check the length before exporting
  return(index(series[idx]))
}



#' Generate vectorized var/cov dataframe from individual log returns
#'
#' @param ind.returns daily xts object properly named with individual
#'   log returns, indexed using date
#' @param period string indicating the period in which the vcov is collected.
#'   default: 'monthly', if 'daily', doing rolling apply, getting realized
#'   vcov matrix from the past 21 days
#' @param FUNC function applied on getting vectorirzed matrix
#' @param roll.window integer length of window in rollapply
#' @return xts object properly named with each row being vectorized
#'   realized variance/covariance matrix of that period
#' @description works the same as first getting bivariate returns dataframe,
#'   calculating monthly variance and covariance seperately and mergeing
#' @export
GenVCOV <- function(ind.returns,
                    period = c('monthly', 'daily'),
                    FUNC = NULL,
                    roll.window = 21) {
  # fill the numeric vcov matrix and fill the index
  if (period == 'monthly') {
    vcov.mat <- xts::apply.monthly(ind.returns, FUNC) # FUNC takes a df
    vcov.mat <- zoo::`index<-`(vcov.mat, zoo::as.yearmon(zoo::index(vcov.mat)))
  } else if (period == 'daily') {
    # align right to get realized correlation
    vcov.mat <- ind.returns %>% zoo::rollapply(roll.window, FUNC, by.column = F, align = 'right') %>%
      stats::na.omit()
  }
  `colnames<-`(vcov.mat, GenVCOVName(ind.names = colnames(ind.returns)))
}


#' Generate names for vcov matrix given individual names
#'
#' @param ind.names vector of string, individual names
#' @return vector of string, names for vectorized vcov matrix
#'   following row-first order and only for upper triangular
#'   matrix of the square vcov matrix
#' @description improved version using pure functional programming
#'   without any for loop and step-wise condition expression
#' @examples
#' ind.names <- c("KW", "FO", "BY", "JZ", "DR")
#' GenVCOVName(ind.names)
GenVCOVName <- function(ind.names) {
  n <- length(ind.names)
  vcov.names <- vector('list')
  # use list for better performance and more flexible indexing
  # inplace assignment and 2 ways of indexing using [[]] and []

  # get (i, j) list for (n, n) square matrix for now
  # generate indexes (is and js) for UPPER TRI MATRIX
  is <- unlist(lapply(1:n, function(x) rep(x, n + 1 - x)))
  js <- unlist(lapply(1:n, function(x) x:n))
  ijlist <- mapply(c, is, js, SIMPLIFY = FALSE)
  # same as zip in python, pair of indexes of upper tri of sq mat

  # assign different operations for element on diag or not
  isdiag <- sapply(ijlist, function(x) x[1] == x[2])
  vcov.names[isdiag] <- ind.names
  vcov.names[!isdiag] <- lapply(ijlist[!isdiag], function(x)
    paste(ind.names[x[1]], '&', ind.names[x[2]], sep = ' '))
  # use list for inplace operation, a trick for better performance

  unlist(vcov.names)
}


#' Function Wrapper for xts::apply.monthly
#'
#' @param daily.returns xts object with individual returns
#' @return xts object with monthly log return
#' @description the reason for this function is that xts:apply.monthly
#'   only take one column (or the first column if called with a df), and
#'   xts::to.monthly has outputs with multiple columns (OHLC)
#' @importFrom magrittr %>%
#' @export
GenMonthlyRet <- function(daily.returns) {
  daily.returns %>% apply(2, function(vec) xts::apply.monthly(vec, sum)) %>%
    xts::xts(order.by = zoo::index(xts::to.monthly(daily.returns)))
  # or to.yearmon of zoo::index(daily.returns)
}


#' Generate scaled z scores for volatility spikes
#'
#' @param df xts object of rolling vcov matrix, monthly or daily
#' @return numeric vector of (scaled) spike values
#' @description
#'   # some test about risk spikes
#'   spikes <- stats::na.omit(zoo::rollapply(monthly.vcov[, "SPY"], 36, GenSpike))
#'   spikes.lagged <- stats::na.omit(xts::lag.xts(spikes))["2004/2017"]
#'   longterm.risk <- stats::na.omit(xts::lag.xts(stats::na.omit(zoo::rollapply(monthly.vcov, 11, function(df) colMeans(df))), 2))["2004/2017"]
#'   last.risk <- stats::na.omit(xts::lag.xts(monthly.vcov))["2004/2017"]
#'
#'   summary(lm(monthly.ret[, "SPY"] ~ last.risk[, "SPY"] + longterm.risk[, "SPY"])) # -27.01(-2.860), 34.23(2.171
#'   summary(lm(monthly.ret[, "EFA"] ~ last.risk[, "EFA"] + longterm.risk[, "EFA"])) # -18.64(-2.066), 33.76(2.199
#'   summary(lm(monthly.ret[, "EEM"] ~ last.risk[, "EEM"] + longterm.risk[, "EEM"])) # -6.73(-1.262), 3.48(2.495)
#'
#' @export
GenSpike <- function(df) {
  stopifnot(dim(df)[1] > 12) # require 12 months to gen long term vol level

  sd.spikes <- apply(df, 2, sd) * sqrt(1 + 1/11) # assume iid
  spikes <- df[nrow(df), ] - colMeans(df[(nrow(df)-1) : (nrow(df)-11), ])

  # take the last month, average of -2 to -12 month as longterm # take the difference, as "spikes"
  # scaled using mean = 0, sd = sd in 3 years, to 0-1

  # spikes / sd.spikes # raw z score
  plogis((spikes - 0) / sd.spikes) # scaled using logistic function
}


#' Generate binary output for volatility spikes
#'
#' @param df xts object of rolling vcov matrix, monthly or daily
#' @return binary if the spike is positive
#' @export
IfSpike <- function(df) {
  stopifnot(dim(df)[1] > 12) # require 12 months to gen long term vol level
  as.numeric(df[nrow(df)] > colMeans(df[(nrow(df)-1) : (nrow(df)-11), ]))
}


#' Generate Z Score for a dataframe
#'
#' @export
GenZScore <- function(df) {
  ms <- apply(df, 2, mean)
  sds <- apply(df, 2, stats::sd)
  plogis((df[nrow(df), ] - ms) / sds)
}


#' Generate Feature Scaling score for a dataframe
#'
#' @export
GenFeatScale <- function(df) {
  maxs <- apply(df, 2, max); mins <- apply(df, 2, min)
  (df[nrow(df), ] - mins) / (maxs - mins)
}


#' Generate Empirical Quantile for a vector with its own info
#'
#' @param vec vector/1dxts
#' @return vector/1dxts of empirical quantile based on its emp distr
#' @importFrom magrittr %>%
#' @export
GenEmpQuantileVec <- function(vec) {
  vec %>% as.vector %>% sapply(., stats::ecdf(.)) %>% xts::xts(order.by = zoo::index(vec))
}


#' Generate Quantile using Empirical Distribution for a dataframe
#'
#' @export
GenEmpQuantile <- function(df) {
  apply(df, 2, function(vec) {
    stats::ecdf(vec)(vec[length(vec)])
    # Note: continuous property, the min doesn't equal to 0
  })
}


#' Generate a list for bivariate returns
#'
#' @param ind.returns xts object of individual returns
#' @return list of xts object, each is a pair of returns
#'   extracted from the input ind.returns
GenBivRet <- function(ind.returns) {
  pairs <- combn(1:ncol(ind.returns), 2)
  lapply(1:ncol(pairs), function (i) ind.returns[, pairs[, i]])
}


#' Generate sequential value for the (i, j) point in (n, n) matrix
#'
#' @param n,i,j int matrix dim is (n, n), point pos is (i, j)
#' @return int sequential value starting from upper left, row first,
#'   only for upper triangular matrix of the original matrix.
#'   For examples, the sequential value of (2,2) in (3,3) is 4
GenSeqValue <- function(n, i, j) {stopifnot(j >= i); i*n - sum(0:(i-1)) - (n-j)}

#' If the last observation is higher than mean of overall df
#'
#' @export
IfHigherMean <- function(df) {as.numeric(df[nrow(df)] > apply(df, 2, mean))}

#' If the last observation is higher than median of overall df
#'
#' @export
IfHigherMedian <- function(df) {as.numeric(df[nrow(df)] > apply(df, 2, median))}


#' Vectorize asymmetry of sum of products (version 2)
#'
#' @param raw.df dtaframe or xts object
#' @return vector of redefined asymmetry^3 based on SPY returns
#' @export
VecAsymVCOV3 <- function(raw.df) {
  stopifnot(any(colnames(raw.df) == 'SPY')) # use SPY to judge good/bad
  good.index <- raw.df[, 'SPY'] > 0
  (VecASYM(raw.df[!good.index]) - VecASYM(raw.df[good.index]))^3
}


#' Vectorize asymmetry of sum of products
#'
#' @param raw.df dtaframe or xts object
#' @return vector of redefined asymmetry based on SPY returns
#' @export
VecAsymVCOV2 <- function(raw.df) {
  stopifnot(any(colnames(raw.df) == 'SPY')) # use SPY to judge good/bad
  good.index <- raw.df[, 'SPY'] > 0
  VecASYM(raw.df[!good.index]) - VecASYM(raw.df[good.index])
}


#' Vectorize asymmetry of covariance (pure difference)
#'
#' @param raw.df dataframe or xts object
#' @return vector of vcov asymmetry based on SPY returns
#' @export
VecAsymVCOV <- function(raw.df) {
  stopifnot(any(colnames(raw.df) == 'SPY')) # use SPY to judge good/bad
  good.index <- raw.df[, 'SPY'] > 0
  VecVCOV(raw.df[!good.index]) - VecVCOV(raw.df[good.index])
}


#' Vectorize covariance when the SPY is down (bad)
#'
#' @param raw.df dataframe or xts object
#' @return vector of bad vcov based on SPY returns
#' @export
VecBadVCOV <- function(raw.df) {
  stopifnot(any(colnames(raw.df) == 'SPY')) # use SPY to judge good/bad
  good.index <- raw.df[, 'SPY'] > 0
  VecVCOV(raw.df[!good.index])
}


#' Vectorize the upper tri part of asym matrix
#'
#' @param raw.df dataframe or xts object
#' @return vector of numeric values, sum of pair-wise product
#'   output has the same order of GenVCOVName
#' @export
VecASYM <- function(raw.df) {
  n <- ncol(raw.df)
  # asyms <- vector('list')
  is <- unlist(lapply(1:n, function(x) rep(x, n + 1 - x)))
  js <- unlist(lapply(1:n, function(x) x:n))
  ijlist <- mapply(c, is, js, SIMPLIFY = FALSE)

  # can be applied using one line
  # isdiag <- sapply(ijlist, function(x) x[1] == x[2])
  # asyms[isdiag] <- apply(raw.df, 2, function(x) sum(x^2))
  asyms <- lapply(ijlist, function(x) sum(raw.df[, x[1]] * raw.df[, x[2]]))

  unlist(asyms)
}


#' Vectorize the upper tri part of vcov matrix for a dataframe
#'
#' @param raw.df dataframe or xts object
#' @return vector of numeric values in vectorized vcov matrix
#'   for only the upper triangular part
#' @export
VecVCOV <- function(raw.df) {
  unique(as.vector(cov(raw.df)))
}
