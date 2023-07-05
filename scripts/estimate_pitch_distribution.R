
library(predpitchscore)

pitch_distribution_model <- list()

pitch_distribution_model[["FF"]] <- optimize_pitch_posterior(
  pitch = pitch,
  event = event,
  pt = "FF"
)

saveRDS(pitch_distribution_model[["FF"]], file = "models/distribution/FF.rds")
