
library(predpitchscore)


# Load the data ----

data_dir <- "data"

pitch <- data.table::fread("data/pitch/2022.csv")
event <- read.csv("data/event/2022.csv")


# Fit the models ----

base_out_run_exp <- compute_base_out_run_exp(event)

count_value <- compute_count_value(
  pitch = pitch,
  event = event,
  base_out_run_exp = base_out_run_exp
)

hit_outcome_model <- train_hit_outcome_model(
  pitch = pitch,
  event = event,
  base_out_run_exp = base_out_run_exp
)

pitch$hit_pred[!is.na(pitch$launch_speed)] <- hit_outcome_model$pred
pitch$is_rhb <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  with(bat_side == "R") |>
  as.numeric()

outcome_tree <- get_outcome_tree(pitch$description)
pitch<-cbind(pitch,outcome_tree)

actualpitch_pred<- tibble::tibble(
  pre_balls = pitch$pre_balls,
  pre_strikes = pitch$pre_strikes,
  prob_swing = as.numeric(pitch$is_swing),
  prob_hbp = as.numeric(pitch$is_hbp),
  prob_strike = as.numeric(pitch$is_strike),
  prob_contact = as.numeric(pitch$is_contact),
  prob_fair = as.numeric(pitch$is_fair),
  pred_hit = pitch$hit_pred
)

pitch_value <- compute_pitch_value(pitch_pred = actualpitch_pred, count_value = count_value)
pitch$true_value<-pitch_value$pitch_value


pitch_outcome_model <- train_pitch_outcome_model(pitch = pitch, count_value = count_value)
pitch_stuff_outcome_model <- train_pitch_outcome_model(
  pitch = pitch,
  count_value = count_value,
  stuff_only = TRUE
)


# Save the models ----

write.csv(base_out_run_exp, file = "models/base_out_run_exp.csv", row.names = FALSE)
write.csv(count_value, file = "models/count_value.csv", row.names = FALSE)
saveRDS(hit_outcome_model, file = "models/hit_outcome_model.rds")
saveRDS(pitch_outcome_model, file = "models/pitch_outcome_model.rds")
saveRDS(pitch_stuff_outcome_model, file = "models/pitch_stuff_outcome_model.rds")

