#' Generate Bootstrap Band for KernSmooth
#'
#' @param groups conditional variable series from GenCondGroups
#' @param bw,gs hyperparams for KernSmoothing
#' @param n number of times for boostrapping
#' @param pctg float percentage of total sample for subsample
#' @importFrom magrittr %>%
#' @export
GenBKDEBand <- function(groups, bw = 0.003, gs = 128, n = 100, pctg = 0.8) {
  lapply(groups, function(group) {
    subs <- do.call(rbind, lapply(1:n, function(i) {
      idx <- sample(1:length(group), length(group) * pctg)
      GenBKDE(group[idx], bw, gs, range = c(min(group), max(group)))[[1]] %>%
        {./sum(.)} # specify the range to make sure each subs has identical idx
    }))
    means <- subs %>% apply(2, mean); sds <- subs %>% apply(2, stats::sd)
    cbind('2sd lower' = means - 2*sds, 'mean' = means, '2sd upper' = means + 2*sds)
    # plot(means, type = 'l'); lines(means - 2*sds, col = "blue"); lines(means + 2*sds, col = "red")
  })
}


#' Generate Bootstrap Band for ASH
#'
#' @param groups conditional variable series from GenCondGroups
#' @param nbin number of bin hyperparameter
#' @param n number of times for boostrapping
#' @param pctg float percentage of total sample for subsample
#' @importFrom magrittr %>%
#' @export
GenASHBand <- function(groups, nbin = 128, n = 1000, pctg = 0.8) {
  lapply(groups, function(group) {
    subs <- do.call(rbind, lapply(1:n, function(i) {
      idx <- sample(1:length(group), length(group) * pctg)
      GenASH(group[idx], nbin, range = c(min(group), max(group)))[[1]] %>%
        {./sum(.)} # specify the range to make sure each subs has identical idx
    }))
    means <- subs %>% apply(2, mean); sds <- subs %>% apply(2, stats::sd)
    cbind('2sd lower' = means - 2*sds, 'mean' = means, '2sd upper' = means + 2*sds)
    # plot(means, type = 'l'); lines(means - 2*sds, col = "blue"); lines(means + 2*sds, col = "red")
  })
}


#' Generate density list for groups using BKDE KernSmooth
#'
#' @param groups a list of conditional variables from GenCondGroups
#' @param bw,gs hyperparameters
#' @return list of density series
#' @importFrom magrittr %>%
#' @import KernSmooth
#' @export
GenBKDE <- function(groups, bw = 0.003, gs = 128L, range = NULL) { # how to tune?
  lapply(groups, function(group) {
    if (is.null(range)) {range <- range(groups)}
    bkde(group, "normal", FALSE, bw, gs, range)$y %>%
      {./sum(.)}
  })
}


#' Generate density list for groups using ASH
#'
#' @param groups a list of conditional variables from GenCondGroups
#' @param nbin number of bin hyperparameter
#' @return list of density series
#' @importFrom magrittr %>%
#' @import ash
#' @export
GenASH <- function(groups, nbin = 128, range = NULL) {
  lapply(groups, function(group) {
    if (is.null(range)) {range <- range(groups)}
    ash1(bin1(sample, ab = c(min(group), max(group)), nbin = 128))$y %>%
      {./sum(.)}
  })
}


#' Calculate Square Distance between ith and jth in Dens list
#'
#' @param dens density list generated from GenXXXX
#' @param i,j int
#' @export
DistSQ <- function(dens, i, j) {sum((dens[[i]] - dens[[j]]) ^ 2)}


#' Calculate Utility Risk Distance between ith and jth in Dens list
#'
#' @param dens density list generated from GenXXXX
#' @param i,j int
#' @param idx index of densities, can be generated again out of the funcion
#'   as the index of density is not returned in GenXXXX function
#' @param gamma float parameter of CRRA, not very influential
#' @importFrom magrittr %>%
#' @description asymmetric adjusted weights for 2 tails, higher weights on left
#' @export
DistUR <- function(dens, i, j, idx, gamma = 0.6) {
  U <- seq(1, length(dens[[1]])) %>% {(.^(1-gamma)-1)/(1-gamma)}
  R <- U %>% {abs(.-.[which.min(abs(idx))]) %>% {./sum(.)}}
  sum((dens[[i]] - dens[[j]]) * R)
} # adjusted measure for distance, give more weights to tail, and consider asymmetry
