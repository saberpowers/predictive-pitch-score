
year <- 2022

pitch <- data.table::fread(glue::glue("data/pitch/{year}.csv"))
event <- data.table::fread(glue::glue("data/event/{year}.csv"))

pitch_types <- c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS")
pitch_distrib_model <- list()

cluster <- parallel::makeCluster(parallel::detectCores())
pitch_distrib_model <- parallel::parLapply(
  cl = cluster,
  X = pitch_types,
  fun = function(pt, year, ...) {

    pitch_distrib_model <- try(predpitchscore::train_pitch_distrib_model(pt, ...))

    if (class(pitch_distrib_model) == "try-error") {
      logger::log_warn("{year} {pt} posterior optimization erred; skipping")
      return(pitch_distrib_model)
    }

    file <- glue::glue("models/distribution/{pt}/{year}.rds")

    # I don't like to write to file within a function like this, but saving the cmdstanr fitted
    # model is funky. We can probably improve upon how we handle this.
    # Have to call `$save_object` method first to ensure cmdstan model saves correctly
    pitch_distrib_model$cmdstan_fit$save_object(file = file)
    pitch_distrib_model$cmdstan_fit <- readRDS(file)
    saveRDS(pitch_distrib_model, file = file)

    return(pitch_distrib_model)
  },
  year = year,
  pitch = pitch,
  event = event,
  iter = 15000,
  tol_param = 1e-8
)
names(pitch_distrib_model) <- pitch_types
parallel::stopCluster(cluster)
