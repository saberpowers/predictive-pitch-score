
# This is a rough quadratic approximation trained on a season of pitch data. Drag is supposed
# to be proportional to the square of velocity (as well as air density and ball properties).
# Note that we are treating drag and ay (acceleration in the y direction) as the same thing,
# which is not exactly correct, but again it's an approximation.
approximate_drag <- function(by) {
  ay <- 0.001597 * by^2
  return(ay)
}

# This function infers the full trajectory of a pitch from its release point, release speed, break,
# and plate location. We are assuming constant acceleration over the flight of the ball, but this
# is also assumed in the definitions of horizontal break and induced vertical break. This function
# returns parameters ax, ay, az, bx, by, bz, cx, cy, cz such that the location of the ball at time t
# is given by:
#   x(t) = ax * t^2 / 2 + bx * t + cx
#   y(t) = ay * t^2 / 2 + by * t + cy
#   z(t) = az * t^2 / 2 + bz * t + cz
# In addition to these parameters, we also return the horizontal and vertical release angle and
# the location of the ball at a specified time after release (snapshot_time, defaults to 150ms).
# Mostly this function is inverting the equations found here:
# https://github.com/saberpowers/predictive-pitch-score/blob/main/package/predpitchscore/R/get_trackman_metrics.R
infer_trajectory <- function(release_x,             # feet
                             release_y,             # feet
                             release_z,             # feet
                             release_speed_mph,     # miles per hour
                             horz_break,            # inches
                             induced_vert_break,    # inches
                             plate_x,               # feet
                             plate_y = 17/12,       # feet, front of home plate
                             plate_z,               # feet
                             snapshot_time = 0.15,  # seconds
                             gravity = -32.17,      # feet per second per second
                             convergence_treshold = 1e-5,
                             max_iterations = 100) {

  cx <- release_x
  cy <- release_y
  cz <- release_z
  release_speed <- -1.466667 * release_speed_mph  # convert mph to feet per second

  # Initialize by
  iteration <- 0
  by <- 0
  by_update <- release_speed    # start by guessing that all velocity moves in the y direction

  while (max(abs(by - by_update)) > convergence_treshold) {

    iteration <- iteration + 1
    if (iteration > max_iterations) {
      break
    }

    by <- by_update
    ay <- approximate_drag(by)

    plate_time <- (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2))

    # Find where the ball would have crossed the plate with only gravity acting on it
    plate_x_gravity <- plate_x - horz_break / 12
    plate_z_gravity <- plate_z - induced_vert_break / 12

    # Infer release velocity in the x and z directions
    bx <- (plate_x_gravity - plate_time^2 / 2 - cx) / plate_time
    bz <- (plate_z_gravity - gravity * plate_time^2 / 2 - cz) / plate_time

    # Infer acceleration in the x and z directions
    ax <- 2 * (plate_x - bx * plate_time - cx) / plate_time^2
    az <- 2 * (plate_z - bz * plate_time - cz) / plate_time^2

    # Update by
    by_update <- -sqrt(release_speed^2 - bx^2 - bz^2)
  }

  # Calculate the specific pitch metrics we care about
  horz_rel_angle <- -atan(bx / by) * 180 / pi  # degrees
  vert_rel_angle <- -atan(bz / by) * 180 / pi  # degrees
  snapshot_x <- ax * snapshot_time^2 / 2 + bx * snapshot_time + cx
  snapshot_y <- ay * snapshot_time^2 / 2 + by * snapshot_time + cy
  snapshot_z <- az * snapshot_time^2 / 2 + bz * snapshot_time + cz

  trajectory <- data.frame(
    ax = ax, ay = ay, az = az, bx = bx, by = by, bz = bz, cx = cx, cy = cy, cz = cz,
    plate_time = plate_time, plate_x = plate_x, plate_z = plate_z,
    horz_rel_angle = horz_rel_angle, vert_rel_angle = vert_rel_angle,
    snapshot_x = snapshot_x, snapshot_y = snapshot_y, snapshot_z = snapshot_z
  )

  return(trajectory)
}

# For a given release point, release speed and break, this function finds the full trajectories of
# pitches matching this specification and labels each one as a strike or not (using the rulebook
# definition of the strike zone). This function could be wayyy more efficient it we make the
# assumption that we are looking for a rectangle of input values, which turns out to be the case
# for release angle and for pitch location at X milliseconds after release.
find_strike_trajectories <- function(release_x,                   # feet
                                     release_y,                   # feet
                                     release_z,                   # feet
                                     release_speed_mph,           # miles per hour
                                     horz_break,                  # inches
                                     induced_vert_break,          # inches
                                     snapshot_time = 0.15,        # seconds
                                     strike_zone_left = -17 / 12, # feet
                                     strike_zone_right = 17 / 12, # feet
                                     strike_zone_top = 3.4,       # feet
                                     strike_zone_bottom = 1.6,    # feet
                                     plate_x_grid = seq(from = -2.5, to = 2.5, by = 0.01),
                                     plate_z_grid = seq(from = 0, to = 5, by = 0.01)) {

  # Create a grid of pitch locations for which we'll calculate the full trajectory
  plate_grid <- expand.grid(plate_x = plate_x_grid, plate_z = plate_z_grid)

  # Find the full trajectory for each pitch location in the grid
  possible_trajectory <- infer_trajectory(
    release_x = release_x,
    release_y = release_y,
    release_z = release_z,
    release_speed_mph = release_speed_mph,
    horz_break = horz_break,
    induced_vert_break = induced_vert_break,
    plate_x = plate_grid$plate_x,
    plate_z = plate_grid$plate_z,
    snapshot_time = snapshot_time
  ) |>
    # Label each possible trajectory as a strike or not
    dplyr::mutate(
      is_strike = (plate_x > strike_zone_left) & (plate_x < strike_zone_right) &
        (plate_z < strike_zone_top) & (plate_z > strike_zone_bottom)
    )
  
  return(possible_trajectory)
}

# Paul Skenes splinker
skenes_splinker <- find_strike_trajectories(
  release_x = -2.3,
  release_y = 54.2,
  release_z = 5.5,
  release_speed_mph = 94.3,
  horz_break = -16.4,
  induced_vert_break = 1.8
) |>
  dplyr::filter(is_strike) |>
  dplyr::summarize(
    min_horz_rel_angle = min(horz_rel_angle),
    max_horz_rel_angle = max(horz_rel_angle),
    min_vert_rel_angle = min(vert_rel_angle),
    max_vert_rel_angle = max(vert_rel_angle),
    min_snapshot_x = min(snapshot_x),
    max_snapshot_x = max(snapshot_x),
    min_snapshot_z = min(snapshot_z),
    max_snapshot_z = max(snapshot_z),
  )

skenes_splinker |>
  tidyr::pivot_longer(cols = dplyr::everything())
#  name                value
#  <chr>               <dbl>
#1 min_horz_rel_angle  2.26 
#2 max_horz_rel_angle  5.17 
#3 min_vert_rel_angle -1.53 
#4 max_vert_rel_angle  0.336
#5 min_snapshot_x     -1.66 
#6 max_snapshot_x     -0.610
#7 min_snapshot_z      4.61 
#8 max_snapshot_z      5.28 
