
devtools::load_all("package/predpitchscore")

year <- 2023
split_even_odd <- FALSE
version <- "conditional"
iter <- 15000
tol_param <- 1e-8
verbose <- TRUE

if (verbose) {
  logger::log_info(
    "Running with year: {year}, version: {version}, iter: {iter}, tol_param: {tol_param}"
  )
}


# Load the data ----

if (verbose) {
  logger::log_info("Loading data")
}

pitch <- data.table::fread(glue::glue("data/pitch/{year}.csv"))
event <- data.table::fread(glue::glue("data/event/{year}.csv"))

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(pitch_type %in% c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS")) |>
  dplyr::mutate(
    training_sample = dplyr::case_when(
      !split_even_odd ~ as.character(year),
      split_even_odd & game_id %% 2 == 0 ~ glue::glue("{year}_even"),
      split_even_odd & game_id %% 2 == 1 ~ glue::glue("{year}_odd")
    ),
    batch = paste(pitch_type, training_sample, sep = "_")
  )


# Set up helper variables for parallel computation ----

num_workers <- parallel::detectCores()

batch_info <- data |>
  dplyr::count(batch, pitch_type, training_sample) |>
  # Assign workers to batches with snake draft order (for computational efficiency)
  dplyr::arrange(-n) |>
  dplyr::mutate(worker = rep(c(1:num_workers, num_workers:1), length = dplyr::n())) |>
  dplyr::arrange(worker)

for (b in 1:nrow(batch_info)) {

  # Load previously estimated complete model (if necessary)
  if (version == "conditional") {
    model_file <- glue::glue("{batch_info$pitch_type[b]}/{batch_info$training_sample[b]}.rds")
    complete_model <- readRDS(glue::glue("models/distribution/complete/{model_file}"))
  } else {
    complete_model <- NULL
  }

  batch_args[[batch_info$batch[b]]] <- list(
    data = data |>
      dplyr::filter(batch == batch_info$batch[b]),
    complete_model = complete_model
  )
}


# Fit the models ----

if (verbose) {
  logger::log_info("Fitting models")
}

future::plan(strategy = future::multisession, workers = parallel::detectCores())
pitch_distrib_model <- future.apply::future_lapply(
  X = batch_args,
  FUN = function(args, ...) {
    model <- try(
      train_pitch_distrib_model(
        data = args$data,
        complete_model = args$complete_model,
        ...
      )
    )
    return(model)
  },
  future.seed = TRUE,
  version = version,
  iter = iter,
  tol_param = tol_param
)
future::plan(strategy = future::sequential)


# Save the models ----

if (verbose) {
  logger::log_info("Saving models")
}

for (b in 1:nrow(batch_info)) {
  saveRDS(
    pitch_distrib_model[[batch_info$batch[b]]],
    file = glue::glue(
      "models/distribution/{version}/{batch_info$pitch_type[b]}/{batch_info$training_sample[b]}.rds"
    )
  )
}
