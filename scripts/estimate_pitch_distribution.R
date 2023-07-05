
library(predpitchscore)

pitch <- data.table::fread("data/pitch/2017.csv")
event <- data.table::fread("data/event/2017.csv")

pitch_distribution_model <- list()

pitch_distribution_model[["FF"]] <- optimize_pitch_posterior(
  pitch = pitch,
  event = event,
  pt = "FF"
)

saveRDS(pitch_distribution_model[["FF"]], file = "models/distribution/FF.rds")
