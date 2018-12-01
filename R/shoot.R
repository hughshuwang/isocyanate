#' Trans Shrinked List to Group Matrix
#'
#' @param tmp list of vectors representing group info
#' @return matrix showing pair-wise group/list info
#' @importFrom magrittr %>%
#' @export
ShlistToGroupMat <- function(tmp) {
  lapply(1:9, function(ele) {
    lapply(1:length(tmp), function(i) ele %in% tmp[[i]])
      1:9 %in% tmp[lapply(1:length(tmp), function(i)
        {ele %in% tmp[[i]]}) %>% unlist][[1]]
  }) %>% {do.call(rbind, .)} * 1
}


#' Trans Group Matrix to Shrinked List
#'
#' @param matrix showing pair-wise group/list info
#' @return shrinked list of vectors
#' @importFrom magrittr %>%
#' @export
GroupMatToShlist <- function(mat) {
  apply(mat, 2, function(vec) {
    (1:9)[as.logical(vec)]
  }) %>% unique
}


#' Generate Binary Shrinked HC Clusters
#'
#' @param merge matrix from hclust, hc$merge
#' @return a list of 2 vectors, showing subgroups in shrinked groups
#' @importFrom magrittr %>%
#' @export
GenBinShHC <- function(merge) {
  n <- nrow(merge)
  sh.list <- as.list(merge[n,])
  while(!all(unlist(sh.list) < 0)) {
    sh.list <- sh.list %>% lapply(function(x) {
      lapply(x, function(x) ifelse(x>0, list(merge[x,]), list(x))) %>% unlist
    })
  }
  lapply(sh.list, function(vec) -vec)
}


#' Generate Binary Shrinked Groups' Densities
#'
#' @param dens list of densities of subgroups
#' @param shlist list of shrinked groups, from GenBinShHC
#' @return list of densities in shrinked groups (default 2)
#' @importFrom magrittr %>%
#' @export
GenBinShDens <- function(dens, shlist) {
  lapply(seq_along(shlist), function(i) {
    dens[shlist[[i]]] %>% {colMeans(do.call(rbind, .))}
  })
}


#' Generate HC results for Shooting a Bullet for a Target
#'
#' @param bullet,target xts time series object, might have diff period
#' @return HC results
#' @importFrom magrittr %>%
#' @export
GenHC <- function(bullet, target) {
  bools <- bullet %>% GenBoolSignal(n.group = 9) # cuts = c(0, 1/10, 3/10, 0.5, 1-3/10, 1-1/10, 1))
  groups <- target %>% na.omit %>% CutSeriesQuantile %>% GenCondGroups(bools)
  fitted.bkde <- GenBKDE(groups, bw = 0.002, gs = 128)
  dens <- fitted.bkde$dens
  axis <- fitted.bkde$axis
  # bbands <- GenBKDEBand(groups, bw = 0.003, n = 1000); bbnoise <- lapply(bbands, function(df) df[, 2] - df[, 1])

  sapply(1:length(groups), function(i) {sapply(1:length(groups), function(j)
    {DistUR(dens, i, j, axis)})}) %>% as.dist %>%
    {hclust(.*100, method = "average")}
  # png(file = "./images/tmp.png", bg = "white"); plot(hc); dev.off()
  # hc$height %>% {.[(length(.) - (n.top - 1)):length(.)]}
}


#' Generate conditional groups based on bool signals and a variable/return xts
#'
#' @param ret xts variable/return
#' @param bools a 2d xts bool, Vector{1dxts{bool}}
#' @return a list of 1dxts, same size as bools
#' @importFrom magrittr %>%
#' @export
GenCondGroups <- function(ret, bools) {
  lapply(bools, function(bool) {xts::merge.xts(ret, bool) %>% stats::na.omit() %>%
      `colnames<-`(c('ret', 'idx')) %>% {.$ret[.$idx != 0]}})
}


#' Generate bool signal vector for a given continuous 0-1 (quantile) signal
#'
#' @param signal xts numerical from 0 to 1, i.e. from GenEmpQuantileVec
#' @param n.group int number of groups divided
#' @param cuts vector of number used to divide groups
#' @examples
#'   GenBoolSignal(quantiles, 6, cuts = c(0, 1/10, 3/10, 0.5, 1-3/10, 1-1/10, 1))
#'   GenBoolSignal(quantiles, 10)
#' @importFrom magrittr %>%
#' @export
GenBoolSignal <- function(signal, n.group = 10, cuts = seq(0, 1, 1/n.group)) {
  stopifnot(range(signal)[1] >= 0 && range(signal)[2] <= 1)
  lapply(1:n.group, function(i) {
    (signal < cuts[i+1] & signal >= cuts[i]) %>% xts::xts(zoo::index(signal))
  }) # bool 2d signals with same index as the numeric signal and MIGHT HAVE NAs
}


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
  if (is.null(range)) {range <- range(groups)} # use range in a whole
  list(
    "dens" = lapply(groups, function(group) {
        bkde(group, "normal", FALSE, bw, gs, range)$y %>% {./sum(.)}
    }),
    "axis" = bkde(groups[[1]], "normal", FALSE, bw, gs, range)$x
  )
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
  sum((dens[[i]] - dens[[j]]) ^ 2 * R)
} # adjusted measure for distance, give more weights to tail, and consider asymmetry
