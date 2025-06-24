
devtools::load_all("package/predpitchscore")

# Parse command line options ----

opt_list <- list(
  optparse::make_option(c("-y", "--year"), type = "integer", default = 2022),
  optparse::make_option(c("-m", "--model_version"), type = "character", default = "complete"),
  optparse::make_option(c("-s", "--split_even_odd"), action = "store_true", default = FALSE),
  optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE)
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = opt_list))


# Load data and models ----

pitch <- data.table::fread(glue::glue("data/pitch/mlb/{opt$year}.csv"))
event <- data.table::fread(glue::glue("data/event/mlb/{opt$year}.csv"))

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(!is.na(extension)) |>
  dplyr::mutate(
    is_rhb = as.numeric(bat_side == "R"),
    training_sample = dplyr::case_when(
      !opt$split_even_odd ~ as.character(year),
      opt$split_even_odd & game_id %% 2 == 0 ~ glue::glue("{opt$year}_even"),
      opt$split_even_odd & game_id %% 2 == 1 ~ glue::glue("{opt$year}_odd")
    )
  ) |>
  get_quadratic_coef() |>
  get_trackman_metrics()

pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
stuff_model <- readRDS("models/stuff_model.rds")
  
data$desc <- predict.pitch_outcome_model(pitch_outcome_model, newpitch = data)$pitch_value
data$stuff <- predict(
    stuff_model,
    newdata = data |>
      dplyr::select(dplyr::all_of(config_pitch_outcome_xgb$features_stuff)) |>
      as.matrix()
  )

player <- jsonlite::fromJSON("https://statsapi.mlb.com/api/v1/sports/1/players")$people |>
  dplyr::transmute(player_id = id, player_name = paste(useName, useLastName, sep = " "))


# Simulate predictive pitch scores ----

pitch_types <- c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS")
training_samples <- unique(data$training_sample)

for (ts in training_samples) {

  leaderboard_pred <- NULL

  for (pt in pitch_types) {

    if (opt$verbose) {
      logger::log_info("Simulating {ts} {pt} predictive pitch scores")
    }

    pitch_distrib_model <- readRDS(
      glue::glue("models/distribution/{opt$model_version}/{pt}/{ts}.rds")
    )

    context <- data |>
      dplyr::filter(training_sample == ts, pitch_type == pt)
    context_list <- split(context, f = context$pitcher_id)
    covered_pitchers <- intersect(
      names(context_list),
      as.character(pitch_distrib_model$pitcher_hand$pitcher_id)
    )
    context_list <- context_list[covered_pitchers]
    pitcher_id_list <- as.list(as.integer(names(context_list)))

    future::plan(strategy = future::multisession, workers = parallel::detectCores())
    pred <- future.apply::future_mapply(
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
          training_sample = ts,
          pred = pred
        )
      )
  }   # end for (pt in pitch_types)


  # Combine predictions and export leaderboard ----
  
  leaderboard <- data |>
    dplyr::filter(training_sample == ts) |>
    dplyr::group_by(pitcher_id, pitch_type) |>
    dplyr::summarize(
      n = dplyr::n(),
      stuff_sd = sd(stuff),
      desc_sd = sd(desc),
      diff_sd = sd(desc - stuff),
      stuff = mean(stuff),
      desc = mean(desc),
      .groups = "drop"
    ) |>
    dplyr::left_join(leaderboard_pred, by = c("pitcher_id", "pitch_type")) |>
    dplyr::left_join(player, by = c("pitcher_id" = "player_id")) |>
    dplyr::select(
      pitcher_id, player_name, pitch_type, n, stuff, desc, pred, stuff_sd, desc_sd, diff_sd
    )

  write.csv(leaderboard, row.names = FALSE, file = glue::glue("output/leaderboard/{ts}.csv"))
}     # end for (ts in training_samples)

logger::log_info("Done")
