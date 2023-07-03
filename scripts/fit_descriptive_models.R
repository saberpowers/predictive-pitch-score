
library(predpitchscore)


# Load the data ----

data_dir <- "data"

pitch <- data.table::fread("data/pitch/2022.csv",
  select = c("game_id", "event_index",
    "launch_speed", "launch_angle", "hit_coord_x", "hit_coord_y",
    "pre_balls", "pre_strikes", "ax", "ay", "az", "vx0", "vy0", "vz0", "x0", "z0", "extension",
    "description"
  )
)

event <- read.csv("data/event/2022.csv")


# Fit the models ----

base_out_run_exp <- compute_base_out_run_exp(event)

hit_outcome_model <- train_hit_outcome_model(
  pitch = pitch,
  event = event,
  base_out_run_exp = base_out_run_exp
)

pitch$hit_pred[!is.na(pitch$launch_speed)] <- hit_outcome_model$pred

pitch_outcome_model <- train_pitch_outcome_model(pitch)


# Save the models ----

saveRDS(hit_outcome_model, file = "models/hit_outcome_model.rds")
saveRDS(pitch_outcome_model, file = "models/pitch_outcome_model.rds")
