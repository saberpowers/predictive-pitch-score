
devtools::load_all("package/predpitchscore")

pitch <- data.table::fread("data/pitch/2022.csv")
event <- data.table::fread("data/event/2022.csv")

stuff_model <- readRDS("models/stuff_model.rds")
pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
pitch_distrib_model <- readRDS("models/distribution/FF/2022.rds")

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(!is.na(extension), pitch_type == "FF") |>
  dplyr::mutate(is_rhb = as.numeric(bat_side == "R"))

pred <- predict.pitch_outcome_model(pitch_outcome_model, newpitch = data) |>
  tibble::add_column(pitcher_id = data$pitcher_id, .before = 1)

pred$stuff <- predict(
    stuff_model,
    newdata = data |>
      get_quadratic_coef() |>
      get_trackman_metrics() |>
      dplyr::select(dplyr::all_of(config_pitch_outcome_xgb$features_stuff)) |>
      as.matrix()
  )

pitcher_ranking <- pred |>
  dplyr::group_by(pitcher_id) |>
  dplyr::summarize(pitches = dplyr::n(), desc_pitch_score = mean(pitch_value), stuff = mean(stuff)) |>
  dplyr::inner_join(pitch_distrib_model$pitcher_hand, by = "pitcher_id") |>
  dplyr::arrange(desc_pitch_score) |>
  dplyr::filter(pitches > 100)

.time <- Sys.time()
# Calculate the predictive pitch score for all pitchers in `pitcher_ranking`
future::plan(strategy = future::multisession, workers = parallel::detectCores())
pitcher_ranking$pred_pitch_score <- future.apply::future_lapply(
  X = as.list(pitcher_ranking$pitcher_id),
  FUN = simulate_pitch_score,
  n = 10000,
  context = data |>
    dplyr::select(bat_side, pre_balls, pre_strikes, strike_zone_top, strike_zone_bottom),
  pitch_distrib_model = pitch_distrib_model,
  pitch_outcome_model = pitch_outcome_model,
  future.seed = TRUE
)
future::plan(strategy = future::sequential)
print(Sys.time() - .time)

pitcher_ranking$pred_pitch_score <- sapply(pitcher_ranking$pred_pitch_score, function(x) x[7])

{
  pdf("figures/temp.pdf")
  with(pitcher_ranking,
    plot(
      x = 2000 * desc_pitch_score,
      y = 2000 * pred_pitch_score,
      type = "n",
      xlab = "FF Descriptive Pitch Score (RAA per 2000 pitches)",
      ylab = "FF Predictive Pitch Score (RAA per 2000 pitches)",
      axes = FALSE
    )
  )
  with(pitcher_ranking,
    text(
      x = 2000 * desc_pitch_score,
      y = 2000 * pred_pitch_score,
      label = pitcher_id,
      cex = 0.5,
      col = ifelse(pitches > 200, "dodgerblue", "darkorange")
    )
  )
  axis(1)
  axis(2)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  dev.off()
}

# ----

data_pitcher <- data |>
  dplyr::filter(pitcher_id == 656302, pre_balls == 0, pre_strikes == 0) |>
  dplyr::mutate(
    strike_zone_top = mean(strike_zone_top),
    strike_zone_bottom = mean(strike_zone_bottom),
    bat_side = "R"
  ) |>
  get_quadratic_coef() |>
  get_trackman_metrics()

sim_pitcher <- simulate_pitches(
  pitcher_id = 656302,
  n = 10000,
  context = data_pitcher,
  model = pitch_distrib_model
) |>
  get_trackman_metrics()


id <- 665645
for (side in c("L", "R")) {
  png(glue::glue("figures/distrib/{id}_FF_{side}.png"), width = 666, height = 666)
  par(mfrow = c(3, 4), mar = c(1, 1, 1, 1))
  for (strikes in 0:2) {
    for (balls in 0:3) {
      visualize_pitch_distrib(
        model = pitch_distrib_model,
        pitcher_id = id,
        data = data |>
          dplyr::filter(
            pitcher_id == id,
            bat_side == side,
            pre_balls == balls,
            pre_strikes == strikes
          ),
        bat_side = side,
        pre_balls = balls,
        pre_strikes = strikes
      )
    }
  }
  dev.off()
}
