#' Stability classification of a mass-spring-damper
#'
#' @inheritParams msd_system
#'
#' @return A list with \code{stable}, \code{marginally_stable},
#'   \code{stability_margin} (largest negative real part) and \code{regime}.
#' @export
msd_stability <- function(m,k,c) {
  info <- msd_eigen(m, k, c)
  re <- Re(info$eigenvalues)
  tol <- 1e-10
  stable <- all(re < -tol)
  marginally_stable <- !stable&&all(re <= tol)
  list(
    stable = stable,
    marginally_stable = marginally_stable,
    stability_margin = -max(re),
    eigenvalues = info$eigenvalues,
    regime = info$regime
  )
}

#Ce que cette fonction fait :
#____
