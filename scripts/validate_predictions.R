
leaderboard <- NULL

for (year in 2021:2022) {
  for (even_odd in c("even", "odd")) {
    leaderboard <- read.csv(glue::glue("output/leaderboard/{year}_{even_odd}.csv")) |>
      tibble::add_column(year = year, even_odd = even_odd, .before = 1) |>
      dplyr::filter(!is.na(pred)) |>
      dplyr::bind_rows(leaderboard)
  }
}

leaderboard_with_noise_var <- leaderboard |>
  dplyr::group_by(year, pitch_type) |>
  dplyr::mutate(
    desc_var = weighted.mean(desc_sd^2, w = n),
    desc_noise_var = desc_var / n,
    stuff_var = weighted.mean(stuff_sd^2, w = n),
    stuff_noise_var = stuff_var / n,
    diff_var = weighted.mean(diff_sd^2, w = n),
    diff_noise_var = diff_var / n
  ) |>
  dplyr::ungroup()

pop_distrib <- leaderboard_with_noise_var |>
  dplyr::group_by(year, pitch_type) |>
  dplyr::summarize(
    desc_pop_mean = weighted.mean(desc, w = 1 / desc_noise_var),
    desc_pop_var = estimate_population_variance(observed_value = desc, noise_variance = desc_noise_var),
    stuff_pop_mean = weighted.mean(stuff, w = 1 / stuff_noise_var),
    stuff_pop_var = estimate_population_variance(observed_value = stuff, noise_variance = stuff_noise_var),
    diff_pop_mean = weighted.mean(desc - stuff, w = 1 / diff_noise_var),
    diff_pop_var = estimate_population_variance(observed_value = desc - stuff, noise_variance = diff_noise_var),
    .groups = "drop"
  )

leaderboard_with_mr <- leaderboard_with_noise_var |>
  dplyr::inner_join(pop_distrib, by = c("year", "pitch_type")) |>
  dplyr::mutate(
    desc_mr = (desc / desc_noise_var + desc_pop_mean / desc_pop_var) /
      (1 / desc_noise_var + 1 / desc_pop_var),
    stuff_mr = (stuff / stuff_noise_var + stuff_pop_mean / stuff_pop_var) /
      (1 / stuff_noise_var + 1 / stuff_pop_var),
    diff_mr = ((desc - stuff) / diff_noise_var + diff_pop_mean / diff_pop_var) /
      (1 / diff_noise_var + 1 / diff_pop_var),
    stuff_desc_mr = stuff_mr + diff_mr
  )

train <- leaderboard_with_mr |>
  dplyr::transmute(
    year, pitcher_id, pitch_type, test = ifelse(even_odd == "even", "odd", "even"),
    n_train = n,
    method_desc = desc,
    method_pred = pred,
    method_desc_mr = desc_mr,
    method_stuff_desc_mr = stuff_desc_mr
  ) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("method_"), names_to = "method", values_to = "prediction")

test <- leaderboard |>
  dplyr::select(year, pitcher_id, pitch_type, even_odd, n_test = n, observation = desc)

data <- train |>
  dplyr::inner_join(test, by = c("year", "pitcher_id", "pitch_type", "test" = "even_odd"))

cor_overall <- data |>
  dplyr::group_by(method) |>
  dplyr::summarize(
    cor = weights::wtd.cor(prediction, observation, w = 1 / (1 / n_train + 1 / n_test))[1],
    sd = weights::wtd.cor(prediction, observation, w = 1 / (1 / n_train + 1 / n_test))[2],
    .groups = "drop"
  )

cor_by_sample_size <- data |>
  dplyr::group_by(method, pitches = 100 * ceiling(n_train / 100)) |>
  dplyr::filter(dplyr::n() > 2) |>
  dplyr::summarize(
    cor = weights::wtd.cor(prediction, observation, w = 1 / (1 / n_train + 1 / n_test))[1],
    sd = weights::wtd.cor(prediction, observation, w = 1 / (1 / n_train + 1 / n_test))[2],
    .groups = "drop"
  )

cor_by_pitch_type <- data |>
  dplyr::group_by(pitch_type, method) |>
  dplyr::filter(dplyr::n() > 2) |>
  dplyr::summarize(
    cor = weights::wtd.cor(prediction, observation, w = 1 / (1 / n_train + 1 / n_test))[1],
    sd = weights::wtd.cor(prediction, observation, w = 1 / (1 / n_train + 1 / n_test))[2],
    .groups = "drop"
  )


write.csv(cor_overall, file = "output/validation/cor_overall.csv", row.names = FALSE)
write.csv(cor_by_sample_size, file = "output/validation/cor_by_sample_size.csv", row.names = FALSE)
