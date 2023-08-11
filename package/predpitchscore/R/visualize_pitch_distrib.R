#' Visualize pitch distribution
#' 
#' Plot a heatmap of the estimated pitch distribution for a specified pitcher and context
#' 
#' @param model a fitted "pitch_distrib_model" object
#' @param pitcher_id integer, specifies which pitcher to visualize
#' @param plot_type character string, specifies type of plot, must be "plate" or "break"
#' @param data optional dataframe of individual pitches to plot as circles
#' @param n number of pitches to sample from the estimated distribution
#' @param bat_side character, must be "L" or "R"
#' @param pre_balls integer, number of balls for context
#' @param pre_strikes integer, number of strikes for context
#' @param strike_zone_top numeric, top of strike zone for context
#' @param strike_zone_bottom numeric, bottom of strike zone for context
#' @param lwd numeric, line width for figure annotations
#' 
#' @return invisible, but a plot is produced
#' 
#' @export
#' 
visualize_pitch_distrib <- function(model,
                                    pitcher_id,
                                    plot_type = c("plate", "break"),
                                    data = NULL,
                                    n = 10000,
                                    bat_side = c("L", "R"),
                                    pre_balls = 0,
                                    pre_strikes = 0,
                                    strike_zone_top = 3.4,
                                    strike_zone_bottom = 1.6,
                                    lwd = 4,
                                    ...) {

  plot_type <- match.arg(plot_type)
  bat_side <- match.arg(bat_side)

  # Draw samples from the pitcher's distribution ----

  context <- data.frame(
    pre_balls = pre_balls,
    pre_strikes = pre_strikes,
    bat_side = bat_side,
    strike_zone_top = strike_zone_top,
    strike_zone_bottom = strike_zone_bottom
  )

  simmed_pitch <- simulate_pitches(
    pitcher_id = pitcher_id,
    n = n,
    context = context,
    model = model
  ) |>
    get_trackman_metrics()

  
  # Create the visualization ----

  if (plot_type == "plate") {
    x <- simmed_pitch$plate_x
    y <- simmed_pitch$plate_z
    h <- 0.6                      # bandwidth for kernal density estimation (chosen to taste)
    lims <- c(-2, 2, 0, 5)        # xlim = (-2, 2) and ylim = (0, 5) for plotting
  } else if (plot_type == "break") {
    x <- simmed_pitch$horz_break
    y <- simmed_pitch$induced_vert_break
    h <- 6                        # bandwidth for kernal density estimation (chosen to taste)
    lims <- c(-25, 25, -25, 25)   # xlim = (-25, 25) and ylim = (-25, 25) for plotting
  }

  # Perform 2-dimensional kernal density estimation
  MASS::kde2d(x = x, y = y, h = h, n = 400, lims = lims) |>   # h = 0.6 was chosen to taste
    # Plot a heatmap of the pitch location distribution
    image(col = viridis::viridis_pal()(400), ...)

  if (plot_type == "plate") {

    # Draw strike zone
    graphics::rect(
      xleft = -17 / 24,
      ybottom = strike_zone_bottom,
      xright = 17 / 24,
      ytop = strike_zone_top,
      border = "white",
      lty = 2,
      lwd = lwd
    )

    # Draw vertical line segment to indicate batter side
    segments(
      x0 = ifelse(bat_side == "R", -1, 1) * 1.67,
      y0 = strike_zone_top + 1,
      y1 = strike_zone_bottom - 1,
      col = "white",
      lwd = lwd
    )

    # Draw home plate to indicate catcher's perspective
    polygon(
      x = c(0, -17/24, -17/24, 17/24, 17/24),
      y = (strike_zone_bottom - 1) * c(0, 1/2, 1, 1, 1/2) / 2,
      border = "white",
      lwd = lwd
    )

  } else if (plot_type == "break") {

    abline(h = 0, col = "white", lty = 2, lwd = lwd)
    abline(v = 0, col = "white", lty = 2, lwd = lwd)
  }

  if (!is.null(data)) {

    x_var_name <- switch(plot_type, "plate" = "plate_x", "break" = "horz_break")
    y_var_name <- switch(plot_type, "plate" = "plate_z", "break" = "induced_vert_break")

    # Attach TrackMan metrics if necessary (depends on which data we've fed in)
    if (!all(c(x_var_name, y_var_name) %in% colnames(data))) {
      data <- data |>
        get_quadratic_coef() |>
        get_trackman_metrics()
    }

    data |>
      dplyr::mutate(
        x = !!rlang::sym(x_var_name),
        y = !!rlang::sym(y_var_name)
      ) |>
      with(points(x = x, y = y, col = "white"))
  }

  return(invisible())
}

