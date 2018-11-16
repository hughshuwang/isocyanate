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
#' # partially modify the signal
#' signal[!(index(signal) %in% stable.idx.efa)] <- 1;
#'
#' @importFrom magrittr %>%
#' @importFrom zoo rollapply
#' @importFrom zoo index
#' @importFrom xts xts
#' @export
GenSubidxSD <- function(series, winlen = 36, sd.threshold = 3, smooth.winlen = 3, IF.LAG = FALSE) {
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


#' Melt one ticker in subgroup list to a vector for density estimations
#'
#' @param subgroup list of xts, with a specific signal condition, each xts df
#'   contains multiple assets and just for a month (cond, signal is monthly)
#' @param ticker string of colname in xts df, for output
#' @param demean bool if demeaned in each month to see deviation
#' @return a vector of merging adjusted (may be demeaned) monthly returns
#' @description
#'   For conditional distributions analysis in nonparametric section
#' @importFrom magrittr %>%
#' @export
MeltSubgroups <- function(subgroup, ticker, demean = TRUE) {
  if (demean) {FUNC <- function(df) df[, ticker] %>% as.vector %>% {.-mean(.)}}
  else {FUNC <- function(df) df[, ticker] %>% as.vector}
  subgroup %>% lapply(FUNC) %>% do.call(c, .)
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
RetToMonthly <- function(daily.returns) {
  daily.returns %>% apply(2, function(vec) xts::apply.monthly(vec, sum)) %>%
    xts::xts(order.by = zoo::index(xts::to.monthly(daily.returns)))
  # or to.yearmon of zoo::index(daily.returns)
}


#' Transfer a xts object to a tibble with date (yearmon/date)
#'
#' @param x xts series, default monthly
#' @param date.range a vector of upper and lower limit,
#'   example for monthly data: "Jan 2010/Jan 2017"
#' @return subsetted tibble dataframe
#' @importFrom magrittr %>%
#' @export
tibbleXTS <- function(x, date.range = NULL) {
  # stopifnot(xts::periodicity(x)$scale == "monthly") # can only take monthly for now
  tbl <- dplyr::as_tibble(x) %>% mutate(date = zoo::index(x)) %>% select(c(ncol(x)+1, 1:ncol(x)))

  # manipulate date.range string for subsetting
  if (!is.null(date.range)) { # if date.range is passed
    date.range <- strsplit(date.range, split = "/")[[1]]

    if (all(lapply(date.range, nchar) %>% unlist == 4)) {
      date.range <- date.range %>% purrr::map(as.yearmon.default) %>% do.call(c, .)
    } # if in year format, 4 digits,
    tbl <- tbl %>% filter(tbl$date >= date.range[1] & tbl$date <= date.range[2])
  }

  tbl
}


#' Check if all signals in a signal pack are in the right range
#'
#' @param signal.pack a list of signals, xts objects
#' @export
CheckSignalRange <- function(signal.pack) {
  stopifnot(all(unlist(lapply(signal.pack, function(signal) max(signal) <= 1)))
            & all(unlist(lapply(signal.pack, function(signal) min(signal) >= 0))))
}


#' Cut range for one single signal
#' @export
CutSignalRange <- function(vec, u, l) {vec[vec<l] <- l; vec[vec>u] <- u; vec}


#' Cut range for one single series using quantiles
#'
#' @param series xts object
#' @param upper,lower numeric, quantiles, i.e. 0.99, 0.01
#' @export
CutSeriesQuantile <- function(series, upper = 1 - 0.001, lower = 0.001) {
  bounds <- c(quantile(series, upper), quantile(series, lower))
  series[series < bounds[2]] <- bounds[2]; series[series > bounds[1]] <- bounds[1]
  series
}


#' Transfer DM and EM (original DRI) signal to US and DM ratio signal
#'
#' @param DE.signal xts series of DM and EM columns (original DRI signal),
#'   ranging from 0 to 1
#' @param us.init,dm.init numeric initial value of US and DM proportions
#' @return list of 2 signals, US and DM, representing US's proportion in
#'   overall portfolio and DM's proportion in international (EE) portfolio
#' @export
DEtoUD <- function(DE.signal, us.init = 0.6, dm.init = 6/7) {
  DM <- DE.signal[, 1]; EM <- DE.signal[, 2]

  US <- DM * dm.init + EM * (1 - dm.init)
  tmp <- (1 - DM) * dm.init * (1 - us.init) / (1 - us.init - US * (1 - us.init))
  DM2 <- (tmp - dm.init) / (1 - dm.init)
  DM2[is.na(DM2)] <- 0
  cbind(`names<-`(US, 'US'), `names<-`(DM2, 'DM'))
}


#' Transfer US and DM ratio signals to DM and EM (original DRI)
#'
#' @export
UDtoDE <- function(UD.signal, us.init = 0.6, dm.init = 6/7) {
  US <- UD.signal[, 1]; DM <- UD.signal[, 2]
  tmp <- 1 - us.init - (1 - us.init) * US

  tmp1 <- tmp * (dm.init + (1 - dm.init) * DM)
  dDM <- 1 - tmp1 / (dm.init * (1 - us.init))
  tmp2 <- tmp * (1- dm.init - (1 - dm.init) * DM)
  dEM <- 1 - tmp2 / ((1 - dm.init) * (1 - us.init))

  cbind(`names<-`(dDM, 'DM'), `names<-`(dEM, 'EM'))
}


#' Compare the start and end dates of two xts objects
#' @export
IfPeriodEqual <- function(A, B) {
  (xts::periodicity(A)$start == xts::periodicity(B)$start) &
    (xts::periodicity(A)$end == xts::periodicity(B)$end)
}
