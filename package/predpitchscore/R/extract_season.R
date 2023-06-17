
extract_season <- function(year, data_dir) {

  schedule <- baseballr::mlb_schedule(season = year) |>
    dplyr::filter(game_type == "R", status_coded_game_state == "F") |>  # final reg season games
    dplyr::arrange(date)
  
  date <- glue::glue("{year}-01-01")
  for (i in 1:nrow(schedule)) {
    
    if (schedule$date[i] != date) {
      date <- schedule$date[i]
      message(date)
    }
  
    game_pk <- schedule$game_pk[i]
    data <- extract_game(game_pk)
  
    write.csv(data, file = glue::glue("{data_dir}/{year}/{game_pk}.csv"), row.names = FALSE)
  }

  return(invisible())
}
