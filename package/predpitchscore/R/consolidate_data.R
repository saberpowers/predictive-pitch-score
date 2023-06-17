
consolidate_data <- function(year, data_dir) {

  files <- list.files(glue::glue("{data_dir}/{year}"))

  data_list <- purrr::map(glue::glue("{data_dir}/{year}/{files}"), data.table::fread)
  data <- do.call(dplyr::bind_rows, data_list) |>
    tibble::as_tibble()

  write.csv(data, file = glue::glue("{data_dir}/consolidated/{year}.csv"), row.names = FALSE)

  return(invisible())
}
