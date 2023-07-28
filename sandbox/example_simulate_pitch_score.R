
library(predpitchscore)

pitch <- data.table::fread("data/pitch/2022.csv")
event <- data.table::fread("data/event/2022.csv")

pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
pitch_distrib_model <- readRDS("models/distribution/FF/2022.rds")

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(pitch_type == "FF")

pred <- predict(pitch_outcome_model, newpitch = data) |>
  tibble::add_column(pitcher_id = data$pitcher_id, .before = 1)

pitcher_ranking <- pred |>
  dplyr::group_by(pitcher_id) |>
  dplyr::summarize(pitches = dplyr::n(), desc_pitch_score = mean(pitch_value)) |>
  dplyr:::filter(pitches > 500) |>
  dplyr::arrange(desc_pitch_score)

# Calculate the predictive pitch score for all pitchers in `pitcher_ranking`
pitcher_ranking$pred_pitch_score <- sapply(
  X = as.list(pitcher_ranking$pitcher_id),
  FUN = simulate_pitch_score,
  n = 10000,
  context = data,
  pitch_distrib_model = pitch_distrib_model,
  pitch_outcome_model = pitch_outcome_model
)

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
