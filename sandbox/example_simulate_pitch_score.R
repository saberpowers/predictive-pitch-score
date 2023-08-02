
library(predpitchscore)

pitch <- data.table::fread("data/pitch/2022.csv")
event <- data.table::fread("data/event/2022.csv")

stuff_model <- readRDS("models/stuff_model.rds")
pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
pitch_distrib_model <- readRDS("models/distribution/FF/2022.rds")

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(pitch_type == "FF") |>
  dplyr::mutate(is_rhb = as.numeric(bat_side == "R"))

pred <- predict.pitch_outcome_model(pitch_outcome_model, newpitch = data) |>
  tibble::add_column(pitcher_id = data$pitcher_id, .before = 1)

stuff <- predict.pitch_outcome_model(stuff_model, newpitch = data) |>
  dplyr::transmute(stuff = pitch_value)

pitcher_ranking <- pred |>
  dplyr::bind_cols(stuff) |>
  dplyr::group_by(pitcher_id) |>
  dplyr::summarize(pitches = dplyr::n(), desc_pitch_score = mean(pitch_value), stuff = mean(stuff)) |>
  dplyr::inner_join(pitch_distrib_model$pitcher_hand, by = "pitcher_id") |>
  dplyr::arrange(desc_pitch_score)

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
      col = "dodgerblue"
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


{
  png("figures/sim_pitcher.png")
  sim_pitcher |>
    with(MASS::kde2d(x = plate_x, y = plate_z, h = 0.6, n = 400, lims = c(-3, 3, 1, 4))) |>
    image(col = viridis::viridis_pal()(400), axes = FALSE)
  graphics::rect(
    xleft = -17 / 12,
    ybottom = mean(data$strike_zone_bottom),
    xright = 17 / 12,
    ytop = mean(data$strike_zone_top),
    border = "white",
    lty = 2,
    lwd = 4
  )
  data_pitcher |>
    with(points(plate_x, plate_z, col = "white"))
  dev.off()
}
