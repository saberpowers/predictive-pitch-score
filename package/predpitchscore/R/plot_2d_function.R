#' Visualize a function of two variables
#' 
#' This function is useful for plotting model fits for supervised regression models with two
#' features. It works best if x and y are laid out on a grid, rather than reflecting data samples.
#' 
#' @param x the first input to the function (x-axis)
#' @param y the second input to the function (y-axis)
#' @param z the output of the function (color)
#' @param z_min the value of z corresponding to the deepest blue, must be <= min(z)
#' @param z_max the value of z corresponding to the deepest red, must be >= max(z)
#' @param pch plotting character, default is 15 (solid square)
#' @param ... additional arguments passed to \code{plot()}
#' 
#' @export
#'
plot_2d_function <- function(x,
                             y,
                             z,
                             z_min = min(z),
                             z_max = max(z),
                             pch = 15,  # solid square
                             ...) {

  # Re-scale the response variable z to be between 0 and 1 (to be translated into a color)
  z_pct <- (z - z_min) / (z_max - z_min)

  plot(
    x = x,
    y = y,
    # z = 0 -> rgb(0, 0, 1) (blue) / z = 0.5 -> rgb(1, 1, 1) (white) / z = 1 -> rgb(1, 0, 0) (red)
    col = rgb(
      red = ifelse(z_pct > 0.5, 1, z_pct * 2),                        # ranges from 0 to 1 to 1
      green = ifelse(z_pct > 0.5, 1 - 2 * (z_pct - 0.5), z_pct * 2),  # ranges from 0 to 1 to 0
      blue = ifelse(z_pct > 0.5, 1 - 2 * (z_pct - 0.5), 1),           # ranges from 1 to 1 to 0
    ),
    pch = pch,
    ...
  )
}
