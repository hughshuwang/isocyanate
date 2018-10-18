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
      GenBKDE(group[idx], 0.003, 128, range = c(min(group), max(group)))[[1]] %>% {./sum(.)}
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
#' @param bw,gs hyperparameters
#' @return list of density series
#' @importFrom magrittr %>%
#' @import ash
#' @export
GenASH <- function(groups, nbin = 128) {
  lapply(groups, function(group) {
    ash1(bin1(sample, ab = c(min(group), max(group)), nbin = 128))$y %>%
      {./sum(.)}
  })
}

