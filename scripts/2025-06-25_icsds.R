
fig_mode <- "dark"

devtools::load_all("package/predpitchscore")

rice_blue <- rgb(0.000, 0.125, 0.357)
rice_gray <- rgb(0.486, 0.494, 0.498)
rice_rich_blue <- rgb(0.039, 0.314, 0.620)


pitch <- data.table::fread("data/pitch/mlb/2022.csv")
event <- data.table::fread("data/event/mlb/2022.csv")

stuff_model <- readRDS("models/stuff_model.rds")
pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")

pitch_distrib_model <- list()
pitch_distrib_model$FF <- readRDS("models/distribution/FF/2022.rds")
pitch_distrib_model$SL <- readRDS("models/distribution/SL/2022.rds")

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(!is.na(extension), balls < 4, strikes < 3) |>
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

{
  sputil::open_device(glue::glue("figures/feature_importance_{fig_mode}.pdf"))
  plot <- importance |>
    dplyr::mutate(
      Feature = stringr::str_to_title(stringr::str_replace_all(Feature, "_", " "))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Feature, Gain), y = Gain)) +
    ggplot2::geom_bar(
      stat = "identity",
      color = sputil::color("blue", fig_mode),
      fill = sputil::color("blue", fig_mode)
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(y = ggplot2::element_blank(), x = ggplot2::element_blank()) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.ticks.y = ggplot2::element_blank()
    )
  print(plot)
  dev.off()
}

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

{
  sputil::open_device(glue::glue("figures/feature_reliability_{fig_mode}.pdf"))
  plot <- reliability |>
    dplyr::mutate(
      Feature = stringr::str_to_title(stringr::str_replace_all(Feature, "_", " "))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Feature, Gain), y = reliability)) +
    ggplot2::geom_bar(
      stat = "identity",
      color = sputil::color("blue", fig_mode),
      fill = sputil::color("blue", fig_mode)
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(y = ggplot2::element_blank(), x = ggplot2::element_blank()) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.ticks.y = ggplot2::element_blank()
    )
  print(plot)
  dev.off()
}

# Create Dylan Cease heatmaps ----

id <- 656302  # Dylan Cease
pt <- "SL"
side <- "R"
balls <- 0
strikes <- 0
for (plot_type in c("plate", "break")) {
  sputil::open_device(glue::glue("figures/distrib/656302_SL_R_0_0_{plot_type}.png"), height = 8.75)
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
  sputil::open_device(glue::glue("figures/distrib/{id}_{pt}_{side}_plate.png"), width = 14.58, height = 14.58)
  par(mfrow = c(3, 4), mar = c(3, 3.1, 0, 0), bg = NA)
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
        lwd = 4,
        axes = FALSE
      )
      if (balls == 0) {
        title(
          ylab = glue::glue("Strikes: {strikes}"),
          cex.lab = 3,
          col.lab = sputil::color("fg", fig_mode),
          line = 1
        )
      }
      if (strikes == 0) {
        title(
          xlab = glue::glue("Balls: {balls}"),
          cex.lab = 3,
          col.lab = sputil::color("fg", fig_mode),
          line = 2
        )
      }
    }
  }
  dev.off()
}


# Produce validation plots ----

cor_by_sample_size <- read.csv("output/validation/cor_by_sample_size.csv")

dodge <- 10
method_order <- c("method_pred", "method_desc_mr", "method_desc")

if (TRUE) {
  sputil::open_device("figures/cor_by_sample_size.pdf", height = 6, width = 8)
  plot <- cor_by_sample_size |>
    dplyr::filter(method %in% method_order) |>
    dplyr::mutate(
      method = factor(method, levels = method_order),
      x = dplyr::case_when(
        method == "method_desc" ~ pitches - dodge,
        method == "method_desc_mr" ~ pitches,
        method == "method_pred" ~ pitches + dodge
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = cor, color = method)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = cor - sd, ymax = cor + sd), width = 10) +
    ggplot2::scale_y_continuous(
      breaks = seq(from = 0.4, to = 1.0, by = 0.1)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        sputil::color("blue", fig_mode),
        sputil::color("green", fig_mode),
        sputil::color("gray", fig_mode)
      ),
      labels = c("Predictive", "Descriptive (MR)", "Descriptive"),
      name = "Method"
    ) +
    ggplot2::coord_cartesian(y = c(0.4, 1.0)) +
    ggplot2::labs(x = "# of Pitches (Training)", y = "Correlation") +
    sputil::theme_sleek() +
    ggplot2::theme(legend.position.inside = c(0.2, 0.9))
  print(plot)
  dev.off()
}
