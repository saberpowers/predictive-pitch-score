
library(predpitchscore)

data_dir <- "data"

data <- data.table::fread(glue::glue("{data_dir}/consolidated/2022.csv")) |>
  tibble::as_tibble()

pitch_outcome_model <- train_pitch_outcome_model(data)
