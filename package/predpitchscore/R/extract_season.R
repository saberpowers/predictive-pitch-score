#' Extract season
#' 
#' Extract all regular season MLB games in a specified year.
#' 
#' @param year an integer specifying the year to extract
#' @param level character string, "mlb" (default) or "aaa"
#' @param cl optional cluster object for parallel computation, default is NULL (not parallel)
#' 
#' @return a list of two dataframes: `event` and `pitch`
#' 
#' @export
#' 
extract_season <- function(year, level = c("mlb", "aaa"), cl = NULL) {

  level <- match.arg(level)
  sport_id <- switch(level, mlb = 1, aaa = 11)

  schedule_filter <- glue::glue("sportId={sport_id}&gameType=R&startDate=01/01/{year}&endDate=12/31/{year}")
  endpoint <- glue::glue("http://statsapi.mlb.com:80/api/v1/schedule?{schedule_filter}")

  schedule_json <- jsonlite::fromJSON(endpoint, flatten = TRUE)

  schedule <- do.call(dplyr::bind_rows, args = schedule_json$dates$games)

  if (is.null(schedule$resumeDate)) {
    schedule$resumeDate <- NA
  }

  game <- schedule |>
    # Filter out non-NA resumeDate to get down to one row per game ID
    dplyr::filter(status.detailedState %in% c("Final", "Completed Early"), is.na(resumeDate)) |>
    dplyr::arrange(officialDate) |>
    dplyr::select(
      game_id = gamePk,
      year = season,
      date = officialDate,
      team_id_away = teams.away.team.id,
      team_id_home = teams.home.team.id,
      venue_id = venue.id
    )

  data_list <- pbapply::pblapply(
    X = game$game_id,
    # If we encounter an error, try a total of three times before returning NULL and moving on
    FUN = function(game_id) {
      is_success <- FALSE
      num_attempts <- 0
      while (!is_success & num_attempts < 3) {
        Sys.sleep(0.1)  # Avoid being rate-limited by statsapi
        data <- try(predpitchscore:::extract_game(game_id))
        if (class(data) == "try-error") {
          num_attempts <- num_attempts + 1
          data <- NULL
          Sys.sleep(5)  # Take a long pause in case it helps avoid network error
        } else {
          is_success <- TRUE
        }
      }
      return(data)
    },
    cl = cl
  )

  event <- do.call(dplyr::bind_rows, args = lapply(data_list, function(x) x$event)) |>
    tibble::add_column(year = year, .after = "game_id")
  pitch <- do.call(dplyr::bind_rows, args = lapply(data_list, function(x) x$pitch)) |>
    tibble::add_column(year = year, .after = "game_id")
  play <- do.call(dplyr::bind_rows, args = lapply(data_list, function(x) x$play)) |>
    tibble::add_column(year = year, .after = "game_id")

  data <- list(
    event = event,
    pitch = pitch,
    play = play,
    game = game
  )
  
  return(data)
}
