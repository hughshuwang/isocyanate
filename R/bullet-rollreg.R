#' Generate monthly beta series from daily returns
#'
#' @param returns xts dataframe with multiple assets, daily returns
#' @param mktcol string of colname of market asset
#' @param intercept bool fit the intercept or not
#' @return beta xts dataframe with yearmon index
#' @importFrom xts apply.monthly
#' @export
MonthlyBeta <- function(returns, mktcol = "SPY", intercept = TRUE) {
  monthly <- apply.monthly(returns, function(df) {
    mkt <- returns[zoo::index(df), mktcol] # select subset for each month
    apply(df, 2, function(vec) {
      if (intercept) {f <- vec~mkt; i <- 2}
      else {f <- vec~mkt+0; i <- 1}
      lm(f)$coefficients[i]
    })
  })
  zoo::index(monthly) <- zoo::as.yearmon(zoo::index(monthly))
  monthly
}


#' Generate monthly alpha series from daily returns
#'
#' @param returns xts dataframe with multiple assets, daily returns
#' @param mktcol string of colname of market asset
#' @return alpha xts dataframe with yearmon index
#' @importFrom xts apply.monthly
#' @export
MonthlyAlpha <- function(returns, mktcol = "SPY") {
  monthly <- apply.monthly(returns, function(df) {
    mkt <- returns[zoo::index(df), mktcol] # select subset for each month
    apply(df, 2, function(vec) {lm(vec~mkt)$coefficients[1]})
  })
  zoo::index(monthly) <- zoo::as.yearmon(zoo::index(monthly))
  monthly
}


#' Generate monthly R-Squared series from daily returns
#'
#' @param returns xts dataframe with multiple assets, daily returns
#' @param mktcol string of colname of market asset
#' @param intercept bool fit the intercept or not
#' @return rsq xts dataframe with yearmon index
#' @importFrom xts apply.monthly
#' @export
MonthlyRSq <- function(returns, mktcol = "SPY", intercept = TRUE) {
  monthly <- apply.monthly(returns, function(df) {
    mkt <- returns[zoo::index(df), mktcol] # select subset for each month
    apply(df, 2, function(vec) {
      if (intercept) {f <- vec~mkt}
      else {f <- vec~mkt+0}
      summary(lm(f))$r.squared
    })
  })
  zoo::index(monthly) <- zoo::as.yearmon(zoo::index(monthly))
  monthly
}


#' Generate monthly last residual series from daily returns
#'
#' @param returns xts dataframe with multiple assets, daily returns
#' @param mktcol string of colname of market asset
#' @param intercept bool fit the intercept or not
#' @return last residual xts dataframe with yearmon index
#' @importFrom xts apply.monthly
#' @export
MonthlyRegres <- function(returns, mktcol = "SPY", intercept = TRUE) {
  monthly <- apply.monthly(returns, function(df) {
    mkt <- returns[zoo::index(df), mktcol] # select subset for each month
    apply(df, 2, function(vec) {
      if (intercept) {f <- vec~mkt; i <- 2}
      else {f <- vec~mkt+0; i <- 1}
      vec[length(vec)] - lm(f)$coefficients[i] * mkt[length(mkt)]
    })
  })
  zoo::index(monthly) <- zoo::as.yearmon(zoo::index(monthly))
  monthly
}


#' Rolling apply fitted beta series of reg: ret~ret.mkt
#'
#' @param returns xts df of returns, one column is the mkt return
#'   default daily, can be monthly
#' @param winlen length rollapply window, default 21 trading days
#' @param mktcol string colname of market, default 'SPY'
#' @param intercept bool fit the intercept or not
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @export
RollingBeta <- function(returns,
                        winlen = 21,
                        mktcol = "SPY",
                        intercept = TRUE) {
  rollapply(returns, winlen, function(df) {
    apply(df, 2, function(vec, mkt) {
      if (intercept) {f <- vec~mkt; i <- 2}
      else {f <- vec~mkt+0; i <- 1}
      lm(f)$coefficients[i]
    }, mkt = df[, mktcol]
  )}, by.column = F) %>% na.omit
}


#' Rolling apply fitted alpha/intcpt series of reg: ret~ret.mkt
#'
#' @param returns xts df of returns, one column is the mkt return
#'   default daily, can be monthly
#' @param winlen length rollapply window, default 21 trading days
#' @param mktcol string colname of market, default 'SPY'
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @export
RollingAlpha <- function(returns,
                         winlen = 21,
                         mktcol = 'SPY') {
  rollapply(returns, winlen, function(df) {
    apply(df, 2, function(vec, mkt) {lm(vec~mkt)$coefficients[1]},
          mkt = df[, mktcol])}, by.column = F) %>% na.omit
}


#' Rolling apply R-squared series of reg: ret~ret.mkt
#'
#' @param returns xts df of returns, one column is the mkt return
#'   default daily, can be monthly
#' @param winlen length rollapply window, default 21 trading days
#' @param mktcol string colname of market, default 'SPY'
#' @param intercept bool fit the intercept or not
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @export
RollingRSq <- function(returns,
                       winlen = 21,
                       mktcol = 'SPY',
                       intercept = TRUE) {

  rollapply(returns, winlen, function(df) {
    apply(df, 2, function(vec, mkt) {
      if (intercept) {f <- vec~mkt}
      else {f <- vec~mkt+0}
      summary(lm(f))$r.square
    }, mkt = df[, mktcol]
  )}, by.column = F) %>% na.omit
}


#' Rolling apply last residual of reg: ret~ret.mkt
#'
#' @param returns xts df of returns, one column is the mkt return
#'   default daily, can be monthly
#' @param winlen length rollapply window, default 21 trading days
#' @param mktcol string colname of market, default 'SPY'
#' @param intercept bool fit the intercept or not
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @export
RollingRegres <- function(returns,
                          winlen = 21,
                          mktcol = 'SPY',
                          intercept = TRUE) {
  rollapply(returns, winlen, function(df) {
    apply(df, 2, function(vec, mkt) {
      if (intercept) {f <- vec~mkt; i <- 2}
      else {f <- vec~mkt + 0; i <- 1}
      vec[length(vec)] - lm(f)$coefficients[i] * mkt[length(mkt)]
    }, mkt = df[, mktcol]
  )}, by.column = F) %>% na.omit
}
