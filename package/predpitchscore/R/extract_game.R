#' Extract game
#' 
#' Extract event and pitch data from the MLB statsapi.
#' 
#' @param game_pk an integer primary key specifying the game to extract (character is okay)
#' 
#' @return a list of two dataframes: `event` and `pitch`
#' 
extract_game <- function(game_pk) {

  endpoint <- glue::glue("https://statsapi.mlb.com/api/v1.1/game/{game_pk}/feed/live")

  data_json <- jsonlite::fromJSON(endpoint)

  # Extract event data ----

  event_data <- data_json$liveData$plays$allPlays

  base_out_state <- track_base_out_state(event_data)

  event <- tibble::tibble(
    game_id = game_pk,
    event_index = event_data$about$atBatIndex,
    inning = event_data$about$inning,
    half_inning = event_data$about$halfInning,
    batter_id = event_data$matchup$batter$id,
    bat_side = event_data$matchup$batSide$code,
    pitcher_id = event_data$matchup$pitcher$id,
    pitch_hand = event_data$matchup$pitchHand$code,
    event = event_data$result$event,
    is_out = event_data$result$isOut,
    runs_on_event = sapply(event_data$runners,
      FUN = function(x) sum(dplyr::coalesce(x$movement$end, "") == "score")
    )
  ) |>
    dplyr::left_join(base_out_state, by = "event_index")


  # Extract pitch data ----

  pitch_data <- do.call(dplyr::bind_rows, args = event_data$playEvents)

  pitch <- tibble::tibble(
    game_id = game_pk,
    event_index = rep(event_data$about$atBatIndex, times = sapply(event_data$playEvents, nrow)),
    pitch_number = pitch_data$pitchNumber,
    play_id = pitch_data$playId,
    is_pitch = pitch_data$isPitch,
    post_balls = pitch_data$count$balls,
    post_strikes = pitch_data$count$strikes,
    description = pitch_data$details$description,
    pitch_type = pitch_data$details$type$code,
    ax = pitch_data$pitchData$coordinates$aX,
    ay = pitch_data$pitchData$coordinates$aY,
    az = pitch_data$pitchData$coordinates$aZ,
    vx0 = pitch_data$pitchData$coordinates$vX0,
    vy0 = pitch_data$pitchData$coordinates$vY0,
    vz0 = pitch_data$pitchData$coordinates$vZ0,
    x0 = pitch_data$pitchData$coordinates$x0,
    z0 = pitch_data$pitchData$coordinates$z0,
    extension = pitch_data$pitchData$extension,
    strike_zone_top = pitch_data$pitchData$strikeZoneTop,
    strike_zone_bottom = pitch_data$pitchData$strikeZoneBottom,
    launch_speed = pitch_data$hitData$launchSpeed,
    launch_angle = pitch_data$hitData$launchAngle,
    hit_coord_x = pitch_data$hitData$coordinates$coordX,
    hit_coord_y = pitch_data$hitData$coordinates$coordY,
  ) |>
  # Get pre-pitch count
  dplyr::group_by(game_id, event_index) |>
  dplyr::mutate(
    pre_balls = dplyr::coalesce(dplyr::lag(post_balls, 1), 0),
    pre_strikes = dplyr::coalesce(dplyr::lag(post_strikes, 1), 0)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(is_pitch) |>
  # We don't need this column once we've filtered on it
  dplyr::select(-is_pitch)

  return(
    list(
      event = event,
      pitch = pitch
    )
  )
}
