#' Get TrackMan metrics for pitch data
#' 
#' Use the quadratic coefficients describing the path of the pitch to calculate TrackMan metrics
#' 
#' @param data dataframe with the following columns: ax, ay, az, bx, by, bz, cx, cy, cz
#' 
#' @return a dataframe with the following columns:
#'   release_x, release_y, release_z, plate_x, plate_z, horz_break, vert_break, induced_vert_break
#' 
get_trackman_metrics <- function(data) {

  trackman_metrics <- data |>
    dplyr::mutate(

      release_x = cx,
      release_y = cy,
      release_z = cz,

      # Calculate plate location
      plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
      # Solve quadratic equation to get the time at which ball reaches front of plate
      plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
      plate_x = ax * plate_time^2 / 2 + bx * plate_time + cx,
      plate_z = az * plate_time^2 / 2 + bz * plate_time + cz,

      # Set up some intermediate variables for calculating breaks
      gravity = -32.17,   # feet per second per second
      plate_x_line = bx * plate_time + cx,
      plate_z_line = bz * plate_time + cz,
      plate_z_gravity = gravity * plate_time^2 / 2 + bz * plate_time + cz,

      # I'm reconstructing these from memory, so not 100% sure they're correct
      horz_break = plate_x - plate_x_line,
      vert_break = plate_z - plate_z_line,
      induced_vert_break = plate_z - plate_z_gravity
    ) |>
    dplyr::select(
      release_x, release_y, release_z, plate_x, plate_z, horz_break, vert_break, induced_vert_break
    )
  
  return(trackman_metrics)
}
