#' Get features for pitch outcome model
#' 
#' We elect to parameterize each pitch based on its characteristics at the plate because these are
#' most directly relevant to the batter. The one exception is extension, which we include because
#' it has no analog at the plate (plate_y is always 17/12).
#' 
#' @param data dataframe with the following columns: ax, ay, az, bx, by, bz, cx, cy, cz
#' 
#' @return a dataframe with the following columns:
#'   plate_x, plate_z, plate_vx, plate_vy, plate_vz, ax, az, az, extension
#' 
get_outcome_model_features <- function(data) {

  outcome_model_features <- data |>
    dplyr::mutate(
      # Calculate plate location
      plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
      # Solve quadratic equation to get the time at which ball reaches front of plate
      plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
      plate_x = ax * plate_time^2 / 2 + bx * plate_time + cx,
      plate_z = az * plate_time^2 / 2 + bz * plate_time + cz,
      plate_vx = ax * plate_time + bx,
      plate_vy = ay * plate_time + by,
      plate_vz = az * plate_time + bz,
      extension = 60.5 - cy
    ) |>
    dplyr::select(plate_x, plate_z, plate_vx, plate_vy, plate_vz, ax, ay, az, extension)
  
  return(outcome_model_features)
}
