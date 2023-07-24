
library(predpitchscore)

year <- 2022


# Load the data ----

pitch <- data.table::fread(glue::glue("data/pitch/{year}.csv"))
event <- data.table::fread(glue::glue("data/event/{year}.csv"))

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(pitch_type %in% c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS")) |>
  dplyr::mutate(
    even_odd = ifelse(game_id %% 2 == 0, "even", "odd"),
    batch = paste(pitch_type, even_odd, sep = "_")
  )


# Set up helper variables for parallel computation ----

num_workers <- parallel::detectCores()

batch <- data |>
  dplyr::count(batch, pitch_type, even_odd) |>
  # Assign workers to batches with snake draft order (for computational efficiency)
  dplyr::arrange(-n) |>
  dplyr::mutate(worker = rep(c(1:num_workers, num_workers:1), length = dplyr::n())) |>
  dplyr::arrange(worker)

batch_data <- split(data, f = data$batch)[batch$batch]


# Fit the models ----

future::plan(strategy = future::multisession, workers = parallel::detectCores())
pitch_distrib_model <- future.apply::future_lapply(
  X = batch_data,
  FUN = function(data, ...) {
    model <- try(train_pitch_distrib_model(data = data, ...))
    return(model)
  },
  future.seed = TRUE,
  iter = 150,
  tol_param = 1e-8
)
future::plan(strategy = future::sequential)


# Save the models ----

for (b in 1:nrow(batch)) {
  saveRDS(
    pitch_distrib_model[[batch$batch[b]]],
    file = glue::glue("models/distribution/{batch$pitch_type[b]}/{year}_{batch$even_odd[b]}.rds")
  )
}
