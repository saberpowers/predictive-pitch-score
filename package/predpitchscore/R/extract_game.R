
extract_game <- function(game_pk) {

  raw_data <- baseballr::mlb_pbp(game_pk)

  data <- raw_data |>
    dplyr::filter(type == "pitch") |>
    dplyr::select(

      # Play identifiers ----
      play_id = playId,
      game_pk,
      inning = about.inning,
      half_inning = about.halfInning,
      at_bat_index = about.atBatIndex,
      pitch_number = pitchNumber,

      # Context ----
      pitcher_id = matchup.pitcher.id,
      pitch_hand = matchup.pitchHand.code,
      batter_id = matchup.batter.id,
      bat_side = matchup.batSide.code,
      balls_start = count.balls.start,
      strikes_start = count.strikes.start,
      outs_start = count.outs.start,
      balls_end = count.balls.end,
      strikes_end = count.strikes.end,
      outs_end = count.outs.end,

      # Pitch data ----
      description = details.description,
      pitch_type = details.type.code,
      ax = pitchData.coordinates.aX,
      ay = pitchData.coordinates.aY,
      az = pitchData.coordinates.aZ,
      vx0 = pitchData.coordinates.vX0,
      vy0 = pitchData.coordinates.vY0,
      vz0 = pitchData.coordinates.vZ0,
      x0 = pitchData.coordinates.x0,
      z0 = pitchData.coordinates.z0,
      extension = pitchData.extension,
      strike_zone_top = pitchData.strikeZoneTop,
      strike_zone_bottom = pitchData.strikeZoneBottom,

      # Hit data ----
      event = result.event,
      launch_speed = hitData.launchSpeed,
      launch_angle = hitData.launchAngle,
      hit_coord_x = hitData.coordinates.coordX,
      hit_coord_y = hitData.coordinates.coordY,
      # If no runner ever reaches a base in the game, the column for that base is missing
      runner_1b_id = dplyr::matches("matchup.postOnFirst.id"),
      runner_2b_id = dplyr::matches("matchup.postOnSecond.id"),
      runner_3b_id = dplyr::matches("matchup.postOnThird.id")
    ) |>
    dplyr::arrange(at_bat_index, pitch_number)

  return(data)
}
