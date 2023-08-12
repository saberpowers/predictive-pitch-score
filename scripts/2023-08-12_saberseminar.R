
devtools::load_all("package/predpitchscore")

rice_blue <- rgb(0.000, 0.125, 0.357)
rice_gray <- rgb(0.486, 0.494, 0.498)
rice_rich_blue <- rgb(0.039, 0.314, 0.620)


pitch <- data.table::fread("data/pitch/2022.csv")
event <- data.table::fread("data/event/2022.csv")

stuff_model <- readRDS("models/stuff_model.rds")
pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")

pitch_distrib_model <- list()
pitch_distrib_model$FF <- readRDS("models/distribution/FF/2022.rds")
pitch_distrib_model$SL <- readRDS("models/distribution/SL/2022.rds")

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(!is.na(extension), pre_balls < 4, pre_strikes < 3) |>
  dplyr::mutate(is_rhb = as.numeric(bat_side == "R")) |>
  get_quadratic_coef() |>
  get_trackman_metrics()

pred_pitch <- predict.pitch_outcome_model(pitch_outcome_model, newpitch = data)


# Get feature importance for pitch outcome model ----

features <- c(
  "plate_x", "plate_z",
  "release_speed", "ind_vert_break", "horz_break",
  "release_x", "release_y", "release_z"
)

covariate_matrix <- data |>
  dplyr::rename(ind_vert_break = induced_vert_break) |>
  dplyr::select(dplyr::all_of(features)) |>
  as.matrix()

model <- xgboost::xgb.train(
  params = list(eta = 0.01, max_depth = 9, min_child_weight = 10, subsample = 0.65, colsample_bytree = 0.7),
  data = xgboost::xgb.DMatrix(data = covariate_matrix, label = pred_pitch$pitch_value),
  nrounds = 1000,
  verbose = 0
)

importance <- xgboost::xgb.importance(model = model)

pdf("figures/feature_importance.pdf")
par(mar = c(2.1, 7.1, 2.1, 2.1))
barplot(
  height = rev(importance$Gain),
  names.arg = importance$Feature |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title() |>
    rev(),
  horiz = TRUE,
  col = rice_blue,
  las = 1,
  axes = FALSE
)
dev.off()

rhp_fastballs <- data |>
  dplyr::filter(pitch_hand == "R", pitch_type == "FF") |>
  dplyr::rename(ind_vert_break = induced_vert_break) |>
  dplyr::select(pitcher_id, dplyr::all_of(features)) |>
  tidyr::pivot_longer(cols = dplyr::all_of(features))

total_var <- rhp_fastballs |>
  dplyr::group_by(Feature = name) |>
  dplyr::summarize(total_var = var(value))

within_pitcher_var <- rhp_fastballs |>
  dplyr::group_by(name, pitcher_id) |>
  dplyr::summarize(n = dplyr::n(), var = var(value), .groups = "drop") |>
  dplyr::filter(n > 30) |>
  dplyr::group_by(Feature = name) |>
  dplyr::summarize(within_pitcher_var = weighted.mean(var, w = n))

reliability <- importance |>
  dplyr::left_join(total_var, by = "Feature") |>
  dplyr::left_join(within_pitcher_var, by = "Feature") |>
  dplyr::mutate(reliability = (total_var - within_pitcher_var) / total_var)

pdf("figures/feature_reliability.pdf")
par(mar = c(2.1, 7.1, 2.1, 2.1))
barplot(
  height = rev(reliability$reliability),
  names.arg = reliability$Feature |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title() |>
    rev(),
  horiz = TRUE,
  col = rice_blue,
  las = 1,
  axes = FALSE
)
dev.off()


# Create Dylan Cease heatmaps ----

id <- 656302  # Dylan Cease
pt <- "SL"
side <- "R"
balls <- 0
strikes <- 0
for (plot_type in c("plate", "break")) {
  png(glue::glue("figures/distrib/656302_SL_R_0_0_{plot_type}.png"), height = 600)
  par(mar = c(0, 0, 0, 0))
  visualize_pitch_distrib(
    model = pitch_distrib_model[[pt]],
    pitcher_id = id,
    plot_type = plot_type,
    data = NULL,
    bat_side = side,
    pre_balls = balls,
    pre_strikes = strikes
  )
  dev.off()
}

id <- 656302  # Dylan Cease
pt <- "SL"
side <- "R"
{
  png(glue::glue("figures/distrib/{id}_{pt}_{side}_plate.png"), width = 1000, height = 999)
  par(mfrow = c(3, 4), mar = c(3, 3.1, 0, 0))
  for (strikes in 2:0) {    # reverse order so that 2 strikes go on top
    for (balls in 0:3) {
      visualize_pitch_distrib(
        model = pitch_distrib_model[[pt]],
        pitcher_id = id,
        plot_type = "plate",
        data = data |>
          dplyr::filter(
            pitcher_id == id,
            pitch_type == pt,
            bat_side == side,
            pre_balls == balls,
            pre_strikes == strikes
          ),
        bat_side = side,
        pre_balls = balls,
        pre_strikes = strikes,
        lwd = 4
      )
      if (balls == 0) {
        title(ylab = glue::glue("Strikes: {strikes}"), cex.lab = 3, line = 1)
      }
      if (strikes == 0) {
        title(xlab = glue::glue("Balls: {balls}"), cex.lab = 3, line = 2)
      }
    }
  }
  dev.off()
}


# Produce validation plots ----

cor_overall <- read.csv("output/validation/cor_overall.csv")
cor_by_sample_size <- read.csv("output/validation/cor_by_sample_size.csv")

plot_cor_overall <- function(cor_overall, num) {
  col <- c(rice_blue, rice_gray, rice_rich_blue, rgb(1, 0.549, 0))
  if (num < 4) {
    col[(num + 1):4] <- rgb(1, 1, 1)
  }
  cor_overall |>
    with(
      Hmisc::errbar(
        x = 1:4,
        y = cor,
        yplus = cor + sd,
        yminus = cor - sd,
        xlab = "",
        ylab = "Correlation",
        axes = FALSE,
        col = col,
        errbar.col = col
      )
    )
  title(
    main = "Out-of-Sample Correlation with Descriptive Model",
    sub = "2021-22 Split Halves"
  )
  axis(1,
    at = 1:num,
    labels = c("Descriptive", "Descriptive (MR)", "Predictive", "Stuff/Desc (MR)")[1:num]
  )
  axis(2)
  return(invisible())
}

{
  pdf("figures/cor_overall_1.pdf", height = 5, width = 7)
  plot_cor_overall(cor_overall = cor_overall, num = 1)
  dev.off()

  pdf("figures/cor_overall_2.pdf", height = 5, width = 7)
  plot_cor_overall(cor_overall = cor_overall, num = 2)
  dev.off()

  pdf("figures/cor_overall_3.pdf", height = 5, width = 7)
  plot_cor_overall(cor_overall = cor_overall, num = 3)
  dev.off()

  pdf("figures/cor_overall_4.pdf", height = 5, width = 7)
  plot_cor_overall(cor_overall = cor_overall, num = 4)
  dev.off()
}

{
  pdf("figures/cor_by_sample_size.pdf", height = 6, width = 8)
  cor_by_sample_size |>
    dplyr::filter(method == "method_desc_mr") |>
    with(
      Hmisc::errbar(
        x = pitches - 5,
        y = cor,
        yplus = cor + sd,
        yminus = cor - sd,
        col = rice_gray,
        errbar.col = rice_gray,
        axes = FALSE,
        xlab = "# of Pitches (Training)",
        ylab = "Correlation",
        ylim = c(0.4, 1)
      )
    )
  cor_by_sample_size |>
    dplyr::filter(method == "method_pred") |>
    with(
      Hmisc::errbar(
        x = pitches + 5,
        y = cor,
        yplus = cor + sd,
        yminus = cor - sd,
        col = rice_rich_blue,
        errbar.col = rice_rich_blue,
        add = TRUE
      )
    )
  title(
    main = "Out-of-Sample Correlation with Descriptive Model",
    sub = "2021-22 Split Halves"
  )
  axis(1, at = seq(from = 100, to = 1000, by = 100))
  axis(2)
  legend("topleft",
    legend = c("Predictive", "Descriptive (MR)"),
    pch = 16,
    lty = 1,
    col = c(rice_rich_blue, rice_gray),
    bty = "n"
  )
  dev.off()
}
