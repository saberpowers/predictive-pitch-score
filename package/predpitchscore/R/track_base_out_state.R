#' Track base-out state
#' 
#' Post-event base-out state is given to us for free. We get pre-event base-out state based on the
#' post-event base-out state of the previous event. However, we also need to get the pre-play
#' base-out state, which describes the base-out state prior to the final pitch of the event.
#' To do so, we start with the pre-event base-out state and account for mid-event runner movement.
#'
#' @param event_data This is the liveData$plays$allPlays section of the JSON from a game's GUMBO feed
#' 
#' @return a tibble with the following columns:
#'  event_index, mid_event_outs, mid_event_runs,
#'  pre_event_runner_1b_id, pre_event_runner_2b_id, pre_event_runner_3b_id, pre_event_out,
#'  pre_play_runner_1b_id, pre_play_runner_2b_id, pre_play_runner_3b_id, pre_play_out,
#'  post_runner_1b_id, post_runner_2b_id, post_runner_3b_id, post_out
#' 
track_base_out_state <- function(event_data) {

  # Step 0. Extract runner data ----

  runners_detail_list <- lapply(event_data$runners, function(x) x$detail)
  runners_movement_list <- lapply(event_data$runners, function(x) x$movement)
  runners_length <- sapply(runners_detail_list, function(x) if(is.null(x)) 0 else nrow(x))
  runners_movement <- dplyr::bind_cols(
    do.call(dplyr::bind_rows, args = runners_detail_list),
    do.call(dplyr::bind_rows, args = runners_movement_list)
  ) |>
    tibble::add_column(event_index = rep(event_data$about$atBatIndex, times = runners_length)) |>
    dplyr::group_by(event_index, play_index = playIndex, runner$id) |>
    dplyr::slice(dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      event_index,
      play_index,
      runner_id = runner$id,
      start_base = dplyr::coalesce(originBase, "batter"),
      end_base = dplyr::coalesce(end, "out"),
      is_out = isOut,
      is_scoring_event = isScoringEvent
    )


  # Step 1. Get the post state ----

  post_state <- tibble::tibble(
    event_index = event_data$about$atBatIndex,
    # We coalesce with NA to handle the case where the value is NULL (e.g. on one reached 3B)
    post_runner_1b_id = dplyr::coalesce(event_data$matchup$postOnFirst$id, NA),
    post_runner_2b_id = dplyr::coalesce(event_data$matchup$postOnSecond$id, NA),
    post_runner_3b_id = dplyr::coalesce(event_data$matchup$postOnThird$id, NA),
    post_outs = event_data$count$outs
  )

  # Step 2. Get the pre-event state ----

  pre_event_state <- post_state |>
    dplyr::transmute(
      event_index,
      pre_event_runner_1b_id = dplyr::lag(post_runner_1b_id, 1),
      pre_event_runner_2b_id = dplyr::lag(post_runner_2b_id, 1),
      pre_event_runner_3b_id = dplyr::lag(post_runner_3b_id, 1),
      pre_event_outs = dplyr::lag(post_outs, 1, default = 0) %% 3
    )

  zombie_runner <- dplyr::coalesce(
    sapply(event_data$playEvents, function(x) any(x$details$event == "Runner Placed On Base")),
    FALSE
  )

  zombie_runner_id <- do.call(dplyr::bind_rows, args = event_data$playEvents) |>
    dplyr::filter(details$event == "Runner Placed On Base") |>
    with(player$id)

  pre_event_state$pre_event_runner_2b_id[zombie_runner] <- zombie_runner_id

  # Step 3. Get the pre-play state (by far the most involved step) ----

  # Find the index of the last play of each event
  event_terminal_play_index <- tibble::tibble(
    event_index = event_data$about$atBatIndex,
    terminal_play_index = sapply(event_data$playEvents, function(x) max(x$index))
  )

  # Create a mid-event runners tibble which excludes runner movement on the final play of the event
  runners_movement_mid_event <- runners_movement |>
    dplyr::inner_join(event_terminal_play_index, by = "event_index") |>
    dplyr::filter(play_index < terminal_play_index) |>
    dplyr::group_by(event_index, runner_id)
    
  runners_movement_mid_event_start <- runners_movement_mid_event |>  
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(event_index, runner_id, start_base)

  runners_movement_mid_event_end <- runners_movement_mid_event |>  
    dplyr::slice(dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(event_index, runner_id, end_base, is_out, is_scoring_event)

  # Count outs and runs scored on mid-event runner movement
  mid_event_outs_runs <- runners_movement_mid_event_end |>
    dplyr::group_by(event_index) |>
    dplyr::summarize(
      mid_event_outs = sum(is_out),
      mid_event_runs = sum(is_scoring_event),
      .groups = "drop"
    )

  # Initiate the pre-play state from the pre-event state (then we'll adjust for mid-event changes)
  pre_play_state <- pre_event_state |>
    dplyr::left_join(mid_event_outs_runs, by = "event_index") |>
    dplyr::transmute(
      event_index,
      # If mid-event outs is NA, that means there weren't any
      mid_event_outs = dplyr::coalesce(mid_event_outs, 0),
      # If mid-event runs is NA, that means there weren't any
      mid_event_runs = dplyr::coalesce(mid_event_runs, 0),
      pre_play_runner_1b_id = pre_event_runner_1b_id,
      pre_play_runner_2b_id = pre_event_runner_2b_id,
      pre_play_runner_3b_id = pre_event_runner_3b_id,
      pre_play_outs = pre_event_outs + mid_event_outs
    )

  for (base in c("1B", "2B", "3B")) {

    id_string <- glue::glue("pre_play_runner_{tolower(base)}_id")

    # If a runner left a base in the middle of a PA, remove the ID from that base
    runners_from_base <- runners_movement_mid_event_start |>
      dplyr::filter(start_base == base)
    pre_play_state[[id_string]][1 + runners_from_base$event_index] <- NA
    # NOTE: event_index is 0-indexed (hence the `1 +`)

    # If a runner reached a base in the middle of a PA, add the runner's ID to that base
    runners_to_base <- runners_movement_mid_event_end |>
      dplyr::filter(end_base == base)
    pre_play_state[[id_string]][1 + runners_to_base$event_index] <- runners_to_base$runner_id
    # NOTE: event_index is 0-indexed (hence the `1 +`)
  }

  # Step 4. Put it all together ----

  base_out_state <- pre_event_state |>
    dplyr::left_join(pre_play_state, by = "event_index") |>
    dplyr::left_join(post_state, by = "event_index") |>
    dplyr::select(
      event_index,
      dplyr::starts_with("mid_event_"),
      dplyr::starts_with("pre_event_"),
      dplyr::starts_with("pre_play_"),
      dplyr::starts_with("post_")
    )

  return(base_out_state)
}
