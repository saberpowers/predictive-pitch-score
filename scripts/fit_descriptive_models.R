
devtools::load_all("package/predpitchscore")

year_string <- "2022:2024"
level <- "mlb"
models_to_fit <- c("hit_outcome",
  "pitch_swing", "pitch_hbp", "pitch_strike", "pitch_contact", "pitch_fair", "pitch_hit",
  "pitch_value", "pitch_stuff"
)
tune <- FALSE
verbose <- TRUE

# Load the data ----

if (verbose) {
  logger::log_info("Loading data")
}

years <- eval(parse(text = year_string))
pitch <- NULL
play <- NULL
event <- NULL

for (year in years) {
  pitch <- data.table::fread(glue::glue("data/pitch/{level}/{year}.csv")) |>
    dplyr::bind_rows(pitch)
  play <- data.table::fread(glue::glue("data/play/{level}/{year}.csv")) |>
    dplyr::bind_rows(play)
  event <- data.table::fread(glue::glue("data/event/{level}/{year}.csv")) |>
    dplyr::bind_rows(event)
}


# Fit the models ----

if (verbose) {
  logger::log_info("Fitting models")
}

base_out_run_exp <- compute_base_out_run_exp(event)

count_value <- compute_count_value(
  play = play,
  event = event,
  base_out_run_exp = base_out_run_exp
)

if ("hit_outcome" %in% models_to_fit) {
  hit_outcome_model <- train_hit_outcome_model(
    pitch = pitch,
    play = play,
    base_out_run_exp = base_out_run_exp,
    tune = tune
  )
} else {
  hit_outcome_model <- readRDS("models/hit_outcome_model.rds")
}

pitch$hit_pred[!is.na(pitch$launch_speed)] <- hit_outcome_model$pred
pitch$is_rhb <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  with(bat_side == "R") |>
  as.numeric()
pitch$true_value <- pitch |>
  cbind(get_outcome_tree(pitch$description)) |>
  dplyr::mutate(
    prob_swing = as.numeric(is_swing),
    prob_hbp = as.numeric(is_hbp),
    prob_strike = as.numeric(is_strike),
    prob_contact = as.numeric(is_contact),
    prob_fair = as.numeric(is_fair),
    pred_hit = dplyr::coalesce(hit_pred, 0)
  ) |>
  compute_pitch_value(count_value = count_value) |>
  with(pitch_value)

if (verbose) {
  logger::log_info("Fitting pitch outcome models")
}

pitch_outcome_model <- train_pitch_outcome_model(
  pitch = pitch,
  count_value = count_value,
  models_to_fit = models_to_fit,   # the extra non-component elements of models_to_fit are ignored
  tune = tune
)

if ("pitch_stuff" %in% models_to_fit) {

  if (verbose) {
    logger::log_info("Fitting stuff model")
  }

  # If we didn't fit all the components of the pitch outcome model, read the model from file so that
  # we can predict pitch value to use as the response variable in the Stuff model.
  if (any(sapply(pitch_outcome_model$xgb, is.null))) {
    pitch_outcome_model_full <- readRDS("models/pitch_outcome_model.rds")
  } else {
    pitch_outcome_model_full <- pitch_outcome_model
  }

  pred_pitch <- predict.pitch_outcome_model(pitch_outcome_model_full, newpitch = pitch)

  stuff_model <- train_stuff_model(
    pitch = pitch,
    pitch_value = pred_pitch$pitch_value,
    tune = tune
  )
}


# Save the models ----

if (verbose) {
  logger::log_info("Saving models")
}

dir <- glue::glue("models/{level}/{year_string}")
if (!exists(dir)) {
  dir.create(dir, recursive = TRUE)
}

write.csv(base_out_run_exp, file = glue::glue("{dir}/base_out_run_exp.csv"), row.names = FALSE)
write.csv(count_value, file = glue::glue("{dir}/count_value.csv"), row.names = FALSE)
if ("hit_outcome" %in% models_to_fit) {
  saveRDS(hit_outcome_model, file = glue::glue("{dir}/hit_outcome_model.rds"))
}
saveRDS(pitch_outcome_model, file = glue::glue("{dir}/pitch_outcome_model.rds"))
if ("pitch_stuff" %in% models_to_fit) {
  saveRDS(stuff_model, file = glue::glue("{dir}/stuff_model.rds"))
}
