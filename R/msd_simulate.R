#' Simulate the free response of a mass-spring-damper
#'
#' Integrates \eqn{\dot{x} = A x} with \pkg{deSolve}.
#'
#' @inheritParams msd_system
#' @param x0 Initial state \code{c(x = 1, v = 0)}.
#' @param times Vector of time points.
#'
#' @return A \code{data.frame} with columns \code{time}, \code{x}, \code{v}.
#' @export
msd_simulate <- function(m,k,c,x0=c(x=1, v=0),
                         times = seq(0, 10, by = 0.01)) {
  if(is.null(names(x0))) names(x0) <- c("x","v")
                                        A <- msd_system(m,k,c)
                                        rhs <- function(t,y,parms) list(as.numeric(A%*%y))
                                        out <- deSolve::ode(y=x0, times = times, func=rhs, parms= NULL)
                                        as.data.frame(out)

}
