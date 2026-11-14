#'State-space matrix of a 1-degree of freedom mass-spring-damper system
#'
#' @param m Mass (must be strictly positive).
#' @param k Stiffness.
#' @param c Damping coefficient.
#'
#' @return A 2x2 numeric matrix
#' @export

msd_system <- function(m,k,c){stopifnot(is.numeric(m),length(m)==1, m>0)
  matrix(c(0,1,-k/m,-c/m), nrow=2, byrow= TRUE)}
