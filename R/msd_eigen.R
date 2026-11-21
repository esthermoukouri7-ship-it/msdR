#' Eigenvalues, damping ratio and regime of a mass-spring-damper
#'
#' @inheritParams msd_system
#'
#' @return A list with components:
#' \describe{
#'   \item{eigenvalues}{Complex vector of eigenvalues of \eqn{A}.}
#'   \item{omega_n}{Natural frequency \eqn{\sqrt{k/m}}.}
#'   \item{zeta}{Damping ratio \eqn{c / (2\sqrt{km})}.}
#'   \item{omega_d}{Damped frequency \eqn{\omega_n\sqrt{1-\zeta^2}}
#'     (only for \eqn{0 \le \zeta < 1}), else \code{NA}.}
#'   \item{regime}{One of \code{"undamped/non_amorti"}, \code{"underdamped/sous_amorti"},
#'   \code{"critically_damped/amorti_critique"}, \code{"overdamped"}, \code{"unstable/instable"}.}
#' }
#' @export
#' @examples
#' msd_eigen(m = 1, k = 1, c = 0.5)$regime

msd_eigen <- function(m,k,c) { stopifnot(length(m)==1, m>0, is.numeric(m))
  wn <- sqrt(k/m)
  zeta <- c/(2*sqrt(k*m))
  ev <- eigen(msd_system(m,k,c))$values

  regime <- if(zeta < 0) {
    "unstable/instable"
  } else if(zeta == 0){
    "undamped/non_amorti"
  } else if(zeta < 1){
    "underdamped/sous_amorti"
  } else if(zeta == 1){
    "critically_damped/amorti_critique"
  } else {
    "overdamped/sur_amorti"
  }

  wd <- if(zeta >= 0 && zeta < 1) wn*sqrt(1 - zeta^2) else NA_real_
  list(eigenvalues=ev, omega_n = wn, zeta = zeta, omega_d = wd, regime=regime)}
