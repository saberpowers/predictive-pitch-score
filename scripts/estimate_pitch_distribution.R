
library(predpitchscore)

pitch <- data.table::fread("data/pitch/2022.csv")
event <- data.table::fread("data/event/2022.csv")

pitch_types <- c("FF", "SI", "SL", "CU", "KC", "CH", "FS", "RC")
pitch_distrib_model <- list()

for (pt in pitch_types) {

  logger::log_info("Estimating {pt} distribution model")

  pitch_distribution_model[[pt]] <- train_pitch_distrib_model(
    pitch = pitch,
    event = event,
    pt = pt
  )

  saveRDS(pitch_distribution_model[[pt]], file = glue::glue("models/distribution/{pt}.rds"))
}
