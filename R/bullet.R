#' Wrap up a general signal
#'
#' @param FUN function used for generating score
#' @param params list of params for FUN
#' @param backtest.period string used for indexing xts
#' @param name name of the signal column
#' @param if.lag bool if lag the signal to the next period
#' @return xts signal series object
#' @importFrom magrittr %>%
#' @export
SignalWrapper <- function(FUN, params, backtest.period, name = 'US', if.lag = TRUE) {
  if (if.lag) {LAG.FUN <- xts::lag.xts} else {LAG.FUN <- xts::xts}
  FUN %>% do.call(params) %>% LAG.FUN() %>% stats::na.omit() %>% `[`(backtest.period) %>% `names<-`(name)
}


#' Cut range for one single series
#'
#' @param series xts object
#' @param upper,lower numeric limits
#' @export
CutSeriesRange <- function(series, upper, lower) {
  series[series < lower] <- lower; series[series > upper] <- upper
  series
}


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
                        rolling.period, burn.in.period = NULL,
                        scoring.method, transfer.method = NULL,
                        output.period = 'monthly') {
  if (data.period == 'daily') {rolling.period <- rolling.period - burn.in.period}
  # adjust for daily input, extract the burn.in.period to make sure the period coverd in rollapply is correct

  score <- data %>% zoo::rollapply(rolling.period, scoring.method) %>% stats::na.omit()

  if (data.period != output.period) { # wrap up outputs
    monthly.index <- unique(zoo::as.yearmon(zoo::index(score))) # save monthly index for latter

    # call transfer method and change index
    # apply the transfer.method on each column of the df for each month
    score <- score %>% xts::split.xts('months') %>% lapply(function(x) apply(x, 2, transfer.method)) %>%
      do.call(rbind, .) %>% xts::xts(monthly.index)
  }

  score
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


#' Generate scaled z scores for volatility spikes
#'
#' @param df xts object of rolling vcov matrix, monthly or daily
#' @return numeric vector of (scaled) spike values
#' @export
GenSpike <- function(df) {
  stopifnot(dim(df)[1] > 12) # require 12 months to gen long term vol level
  sd.spikes <- apply(df, 2, sd) * sqrt(1 + 1/11) # assume iid
  spikes <- df[nrow(df), ] - colMeans(df[(nrow(df)-1) : (nrow(df)-11), ])
  # average of -2 to -12 month as longterm, take the diff as "spikes", scaled using mean = 0, sd = sd in 3 years, to 0-1

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
#' @export
GenZScore <- function(df) {ms <- apply(df, 2, mean); sds <- apply(df, 2, stats::sd); plogis((df[nrow(df),]-ms) / sds)}

#' Generate Feature Scaling score for a dataframe
#' @export
GenFeatScale <- function(df) {maxs <- apply(df, 2, max); mins <- apply(df, 2, min); (df[nrow(df),]-mins) / (maxs-mins)}

#' Generate Quantile using Empirical Distribution for a dataframe
#' @export
GenEmpQuantile <- function(df) {apply(df, 2, function(vec) {stats::ecdf(vec)(vec[length(vec)])})}

#' @export
IfHigherQuantile <- function(df, q) {as.numeric(df[nrow(df)] > apply(df, 2, function(vec) quantile(vec, q)))}

#' @export
IfHigherMean <- function(df) {as.numeric(df[nrow(df)] > apply(df, 2, mean))}

#' @export
IfHigherMedian <- function(df) {as.numeric(df[nrow(df)] > apply(df, 2, median))}


#' Generate sequential value for the "i, j" point in a n squared matrix
#'
#' @param n,i,j int matrix is nth squared, point pos is i,j
#' @return int seq value starting from upper left, row first, only for upper tri
GenSeqValue <- function(n, i, j) {stopifnot(j >= i); i*n - sum(0:(i-1)) - (n-j)}
