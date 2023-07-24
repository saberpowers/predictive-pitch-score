
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
pitch$RHB<-as.numeric(dplyr::left_join(pitch,event, by = c("year", "game_id", "event_index"))$bat_side=="R")
pitch_outcome_model <- train_pitch_outcome_model(pitch = pitch, count_value = count_value, stuff=FALSE)
pitch_stuff_outcome_model <- train_pitch_outcome_model(pitch = pitch, count_value = count_value, stuff=TRUE)


# Save the models ----

write.csv(base_out_run_exp, file = "models/base_out_run_exp.csv", row.names = FALSE)
write.csv(count_value, file = "models/count_value.csv", row.names = FALSE)
saveRDS(hit_outcome_model, file = "models/hit_outcome_model.rds")
saveRDS(pitch_outcome_model, file = "models/pitch_outcome_model.rds")
saveRDS(pitch_stuff_outcome_model, file = "models/pitch_stuff_outcome_model.rds")

