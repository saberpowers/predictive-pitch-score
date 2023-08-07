
devtools::load_all("package/predpitchscore")

year <- 2023
split_even_odd <- FALSE
verbose <- TRUE

# Load data and models ----

pitch <- data.table::fread(glue::glue("data/pitch/{year}.csv"))
event <- data.table::fread(glue::glue("data/event/{year}.csv"))

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(!is.na(extension)) |>
  dplyr::mutate(is_rhb = as.numeric(bat_side == "R")) |>
  get_quadratic_coef() |>
  get_trackman_metrics()

pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
stuff_model <- readRDS("models/stuff_model.rds")


# Simulate predictive pitch scores ----

pitch_types <- c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS")

leaderboard_pred <- NULL

for (pt in pitch_types) {

  if (verbose) {
    logger::log_info("Simulating {pt} predictive pitch scores")
  }

  pitch_distrib_model <- readRDS(glue::glue("models/distribution/complete/{pt}/{year}.rds"))

  context <- data |>
    dplyr::filter(pitch_type == pt)
  context_list <- split(context, f = context$pitcher_id)
  covered_pitchers <- intersect(
    names(context_list),
    as.character(pitch_distrib_model$pitcher_hand$pitcher_id)
  )
  context_list <- context_list[covered_pitchers]
  pitcher_id_list <- as.list(as.integer(names(context_list)))

  future::plan(strategy = future::multisession, workers = parallel::detectCores())
  pred_pitch_score <- future.apply::future_mapply(
    FUN = simulate_pitch_score,
    pitcher_id = pitcher_id_list,
    context = context_list,
    MoreArgs = list(
      n = 10000,
      pitch_distrib_model = pitch_distrib_model,
      pitch_outcome_model = pitch_outcome_model
    ),
    future.seed = TRUE
  )
  future::plan(strategy = future::sequential)

  leaderboard_pred <- leaderboard_pred |>
    dplyr::bind_rows(
      tibble::tibble(
        pitcher_id = as.integer(pitcher_id_list),
        pitch_type = pt,
        pred_pitch_score = pred_pitch_score
      )
    )
}


# Combine predictions and produce leaderboard ----

data$desc_pitch_score <- predict.pitch_outcome_model(pitch_outcome_model, newpitch = data)$pitch_value
data$stuff <- predict(
    stuff_model,
    newdata = data |>
      dplyr::select(dplyr::all_of(config_pitch_outcome_xgb$features_stuff)) |>
      as.matrix()
  )

leaderboard <- data |>
  dplyr::group_by(pitcher_id, pitch_type) |>
  dplyr::summarize(
    n = dplyr::n(),
    desc_pitch_score = mean(desc_pitch_score),
    stuff = mean(stuff),
    .groups = "drop"
  ) |>
  dplyr::left_join(leaderboard_pred, by = c("pitcher_id", "pitch_type"))


# Export leaderboard ----

write.csv(leaderboard, row.names = FALSE, file = glue::glue("output/leaderboard/{year}.csv"))
