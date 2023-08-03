#' Visualize pitch distribution
#' 
#' Plot a heatmap of the estimated pitch distribution for a specified pitcher and context
#' 
#' @param model a fitted "pitch_distrib_model" object
#' @param pitcher_id integer, specifies which pitcher to visualize
#' @param data optional dataframe of individual pitches to plot as circles
#' @param n number of pitches to sample from the estimated distribution
#' @param bat_side character, must be "L" or "R"
#' @param pre_balls integer, number of balls for context
#' @param pre_strikes integer, number of strikes for context
#' @param strike_zone_top numeric, top of strike zone for context
#' @param strike_zone_bottom numeric, bottom of strike zone for context
#' 
#' @return invisible, but a plot is produced
#' 
#' @export
#' 
visualize_pitch_distrib <- function(model,
                                    pitcher_id,
                                    data = NULL,
                                    n = 10000,
                                    bat_side = c("L", "R"),
                                    pre_balls = 0,
                                    pre_strikes = 0,
                                    strike_zone_top = 3.4,
                                    strike_zone_bottom = 1.6,
                                    ...) {

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

  simmed_pitch |>
    # Perform 2-dimensional kernal density estimation
    with(MASS::kde2d(x = plate_x, y = plate_z, h = 0.6, n = 400, lims = c(-2, 2, 0, 5))) |>
    # Plot a heatmap of the pitch location distribution
    image(col = viridis::viridis_pal()(400), axes = FALSE, ...)

  # Draw strike zone
  graphics::rect(
    xleft = -17 / 24,
    ybottom = strike_zone_bottom,
    xright = 17 / 24,
    ytop = strike_zone_top,
    border = "white",
    lty = 2,
    lwd = 2
  )

  # Draw vertical line segment to indicate batter side
  segments(
    x0 = ifelse(bat_side == "R", -1, 1) * 1.67,
    y0 = strike_zone_top + 1,
    y1 = strike_zone_bottom - 1,
    col = "white",
    lwd = 2
  )

  # Draw home plate to indicate catcher's perspective
  polygon(
    x = c(0, -17/24, -17/24, 17/24, 17/24),
    y = (strike_zone_bottom - 1) * c(0, 1/2, 1, 1, 1/2) / 2,
    border = "white",
    lwd = 3
  )

  if (!is.null(data)) {
    data |>
      get_quadratic_coef() |>
      get_trackman_metrics() |>
      with(points(x = plate_x, y = plate_z, col = "white"))
  }

  return(invisible())
}

