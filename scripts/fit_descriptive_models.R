
library(predpitchscore)

year <- 2022
tune <- TRUE
verbose <- TRUE

# Load the data ----

if (verbose) {
  logger::log_info("Loading data")
}

pitch <- data.table::fread(glue::glue("data/pitch/{year}.csv"))
event <- data.table::fread(glue::glue("data/event/{year}.csv"))


# Fit the models ----

if (verbose) {
  logger::log_info("Fitting models")
}

base_out_run_exp <- compute_base_out_run_exp(event)

count_value <- compute_count_value(
  pitch = pitch,
  event = event,
  base_out_run_exp = base_out_run_exp
)

hit_outcome_model <- train_hit_outcome_model(
  pitch = pitch,
  event = event,
  base_out_run_exp = base_out_run_exp,
  tune = tune
)

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
  tune = tune
)

pitch_stuff_outcome_model <- train_pitch_outcome_model(
  pitch = pitch,
  count_value = count_value,
  stuff_only = TRUE
)


# Save the models ----

if (verbose) {
  logger::log_info("Saving models")
}

write.csv(base_out_run_exp, file = "models/base_out_run_exp.csv", row.names = FALSE)
write.csv(count_value, file = "models/count_value.csv", row.names = FALSE)
saveRDS(hit_outcome_model, file = "models/hit_outcome_model.rds")
saveRDS(pitch_outcome_model, file = "models/pitch_outcome_model.rds")
saveRDS(pitch_stuff_outcome_model, file = "models/pitch_stuff_outcome_model.rds")
