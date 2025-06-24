
devtools::load_all("package/predpitchscore")

# Parse command line options ----

opt_list <- list(
  optparse::make_option(c("-y", "--year"), type = "integer", default = 2022),
  optparse::make_option(c("-m", "--model_version"), type = "character", default = "complete"),
  optparse::make_option(c("-s", "--split_even_odd"), action = "store_true", default = FALSE),
  optparse::make_option(c("-i", "--iterations"), type = "integer", default = 15000),
  optparse::make_option(c("-t", "--tolerance"), type = "numeric", default = 1e-8),
  optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE)
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = opt_list))

if (opt$verbose) {
  logger::log_info(
    "Running with year: {opt$year}, split_even_odd: {opt$split_even_odd},",
    " version: {opt$model_version}, iterations: {opt$iterations}, tolerance: {opt$tolerance}"
  )
}


# Load the data ----

if (opt$verbose) {
  logger::log_info("Loading data")
}

pitch <- data.table::fread(glue::glue("data/pitch/mlb/{opt$year}.csv"))
event <- data.table::fread(glue::glue("data/event/mlb/{opt$year}.csv"))

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(pitch_type %in% c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS")) |>
  dplyr::mutate(
    training_sample = dplyr::case_when(
      !opt$split_even_odd ~ as.character(opt$year),
      opt$split_even_odd & game_id %% 2 == 0 ~ glue::glue("{opt$year}_even"),
      opt$split_even_odd & game_id %% 2 == 1 ~ glue::glue("{opt$year}_odd")
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

batch_args <- list()

for (b in 1:nrow(batch_info)) {

  # Load previously estimated complete model (if necessary)
  if (opt$model_version == "conditional") {
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

if (opt$verbose) {
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
  version = opt$model_version,
  iter = opt$iterations,
  tol_param = opt$tolerance
)
future::plan(strategy = future::sequential)


# Save the models ----

if (opt$verbose) {
  logger::log_info("Saving models")
}

for (b in 1:nrow(batch_info)) {
  dir <- file.path("models", "distribution", opt$model_version, batch_info$pitch_type[b])
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(
    pitch_distrib_model[[batch_info$batch[b]]],
    file = file.path(dir, paste0(batch_info$training_sample[b], ".rds"))
  )
}
