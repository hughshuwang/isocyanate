#' Generate Rolling Vectorized Pairwise Matrix
#'
#' @param daily.ret daily xts logret properly named
#' @param period string indicating the period in which the vcov is collected
#' @param FUNC function applied on getting vectorirzed matrix
#' @param winlen integer length of window in rollapply
#' @return xts object named with each row being vectorized realized pairwise matrix
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @import xts
#' @import zoo
#' @export
GenRollPairMatrix <- function(daily.ret, period = c('monthly', 'daily'), FUNC = NULL, winlen = 21) {
  if (period == 'monthly') {df.roll <- apply.monthly(daily.ret, FUNC) %>% {xts(., as.yearmon(index(.)))}}
  else if (period == 'daily') {df.roll <- daily.ret %>% rollapply(winlen, FUNC, by.column = F, align = 'right') %>% na.omit}
  `colnames<-`(df.roll, GenPairName(colnames(daily.ret))) # FUNC and GenPairName are all using GenUpperIJ for col indexing
}


#' Generate index list for upper triangular matrix of n-order squared matrix
GenUpperIJ <- function(n) {
  is <- unlist(mapply(rep, 1:n, n+1-1:n)); js <- unlist(lapply(1:n, function(x) x:n))
  mapply(c, is, js, SIMPLIFY = F) # zip in python, index only for upper tri matrix
}


#' Generate names for pair-wise matrix given individual names
#'
#' @importFrom magrittr %>%
#' @return vector of string, names for vectorized pairwise matrix
#'   following row-first order and only for upper triangular
#'   matrix of the square vcov matrix
GenPairName <- function(names) {
  ijlist <- length(names) %>% GenUpperIJ; isdiag <- sapply(ijlist, function(x) x[1]==x[2]);

  pairnames <- vector('list'); pairnames[isdiag] <- names
  pairnames[!isdiag] <- lapply(ijlist[!isdiag], function(x) paste(names[x[1]], '&', names[x[2]], sep = ' '))

  unlist(pairnames)
}


#' Vectorize asymmetry of sum of products
#' @importFrom magrittr %>%
#' @export
VecAsymSP <- function(df) {stopifnot(any(colnames(df) == 'SPY')); (df[, 'SPY'] > 0) %>% {VecSP(df[!.]) - VecSP(df[.])}}


#' Vectorize asymmetry of covariance by simple difference
#' @export
VecAsymVCOV <- function(df) {stopifnot(any(colnames(df) == 'SPY')); (df[, 'SPY'] > 0) %>% {VecVCOV(df[!.]) - VecVCOV(df[.])}}


#' Vectorize covariance when the SPY is down
#' @export
VecBadVCOV <- function(df) {stopifnot(any(colnames(df) == 'SPY')); (df[, 'SPY'] > 0) %>% {VecVCOV(df[!.])}}


#' Vectorize the upper tri part of vcov matrix for a dataframe
#' @export
VecVCOV <- function(df) {GenUpperIJ(ncol(df)) %>% lapply(function(x) cov(df[, x[1]], df[, x[2]])) %>% unlist}


#' Vectorize Sum of Products for the upper tri matrix
#' @export
VecSP <- function(df) {GenUpperIJ(ncol(df)) %>% lapply(function(x) sum(df[, x[1]] * df[, x[2]])) %>% unlist}


#' Generate a list for bivariate returns
#'
#' @param ret xts object of individual returns
#' @return list of xts object, each is a pair of returns
#'   extracted from the input ind.returns
GenBivRet <- function(ret) {combn(1:ncol(ret), 2) %>% {lapply(1:ncol(.), function (i) ind.returns[, .[, i]])}}
