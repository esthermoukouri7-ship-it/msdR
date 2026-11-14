#' Phase portrait of a mass-spring-damper
#'
#' @inheritParams msd_system
#' @param xlim,ylim Ranges for position and velocity.
#' @param n_grid Number of grid points for the vector field.
#' @param t_max Integration horizon for trajectories.
#'
#' @return A \code{ggplot} object.
#' @export
msd_phase_portrait <- function(m, k, c,
                               xlim = c(-2, 2), ylim = c(-2, 2),
                               n_grid = 15, t_max = 15) {

  grid <- expand.grid(
    x = seq(xlim[1], xlim[2], length.out = n_grid),
    v = seq(ylim[1], ylim[2], length.out = n_grid)
  )

  vel      <- t(apply(grid, 1, function(p) as.numeric(A %*% p)))
  grid$dx  <- vel[, 1]
  grid$dv  <- vel[, 2]

  norms <- sqrt(grid$dx^2 + grid$dv^2)
  norms[norms == 0] <- 1
  scale <- 0.8 * min(diff(xlim), diff(ylim)) / n_grid
  grid$dxn <- grid$dx / norms * scale
  grid$dvn <- grid$dv / norms * scale

  starts <- matrix(
    c( 1,  0, -1,  0,  0.5,  1, -0.5, -1,  1,  1, -1, -1),
    ncol = 2, byrow = TRUE
  )

  trajs <- lapply(seq_len(nrow(starts)), function(i) {
    out <- msd_simulate(m, k, c,
                        x0 = starts[i, ],
                        times = seq(0, t_max, by = 0.05))
    out$id <- factor(i)
    out
  })
  trajs_df <- do.call(rbind, trajs)

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = grid,
      ggplot2::aes(x = x, y = v, xend = x + dxn, yend = v + dvn),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.08, "cm")),
      colour = "pink"
    ) +
    ggplot2::geom_path(
      data = trajs_df,
      ggplot2::aes(x = x, y = v, colour = id),
      linewidth = 0.6, show.legend = FALSE
    ) +
    ggplot2::geom_point(data = data.frame(x = 0, v = 0),
                        ggplot2::aes(x = x, y = v), size = 2.5) +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim) +
    ggplot2::labs(
      x = "Position", y = "Vitesse",
      title = sprintf("Portrait de phase (m=%.2f, k=%.2f, c=%.2f)", m, k, c)
    ) +
    ggplot2::theme_minimal()
}
