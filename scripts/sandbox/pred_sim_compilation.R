library(predpitchscore)
library(readr)

for(year in seq(2017,2022)){
  pred_model <- pred_sim_compilation(year,pitch_outcome_model)
  saveRDS(pred_model,paste("pred_model",year,".rds"))
}

for(year in seq(2017,2022)){
  pred_model <- pred_sim_compilation_full_year(year,pitch_outcome_model)
  saveRDS(pred_model,paste("pred_model_full_season",year,".rds",sep=""))
}

pred_sim_compilation<-function(year, pitch_outcome_model, verbose=TRUE){
  pred_model <- data.frame("pitcher_id"=numeric(0), "pitch_type"=character(0),
                         "even"=logical(0), "count"=numeric(0), "mean_prob_swing"=numeric(0), "mean_prob_hbp"=numeric(0),
                         "mean_prob_strike"=numeric(0),"mean_prob_contact"=numeric(0),"mean_prob_fair"=numeric(0),
                         "mean_pred_hit"=numeric(0),"mean_pitch_value"=numeric(0),"mean_pred_value"=numeric(0))
  pitch <- readr::read_csv(paste("predictive-pitch-score/data/pitch/",year,".csv",sep=""))
  event <- readr::read_csv(paste("predictive-pitch-score/data/event/",year,".csv",sep=""))
  for(pitch_index in c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS")){
    data <- pitch |>
      dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
      dplyr::filter(pitch_type == pitch_index,
                    pre_balls < 4,
                    pre_strikes < 3
      ) |> dplyr::mutate(
        even_odd = ifelse(game_id %% 2 == 0, "even", "odd")
      )
    data$is_rhb <- data |>
      with(bat_side == "R") |>
      as.numeric()
    for(even_odd_index in c("even","odd")){
      if(verbose==TRUE){
        print(paste("Fitting:",pitch_index,even_odd_index,"Time:",Sys.time()))
      }
      pitch_distrib_model <- readRDS(glue::glue("~/Downloads/distribution/{pitch_index}/{year}_{even_odd_index}.rds"))
      data_filtered <- data |>
        dplyr::filter(pitcher_id %in% pitch_distrib_model$pitcher_hand$pitcher_id,
                      even_odd==even_odd_index)
      num_pitchers <- length(pitch_distrib_model$pitcher_hand$pitcher_id)
      new_pred_model <- data.frame("pitcher_id"=numeric(num_pitchers),
                                 "pitch_type"=character(num_pitchers),
                                 "even"=logical(num_pitchers),
                                 "count"=numeric(num_pitchers),
                                 "mean_prob_swing"=numeric(num_pitchers),
                                 "mean_prob_hbp"=numeric(num_pitchers),
                                 "mean_prob_strike"=numeric(num_pitchers),
                                 "mean_prob_contact"=numeric(num_pitchers),
                                 "mean_prob_fair"=numeric(num_pitchers),
                                 "mean_pred_hit"=numeric(num_pitchers),
                                 "mean_pitch_value"=numeric(num_pitchers),
                                 "mean_pred_value"=numeric(num_pitchers))
      new_pred_model[,1] <- pitch_distrib_model$pitcher_hand$pitcher_id
      new_pred_model[,2] <- pitch_index
      new_pred_model[,3] <- even_odd_index=="even"
      new_pred_model[,4] <- NA
      new_pred_model[,5:12] <- t(sapply(
        X = pitch_distrib_model$pitcher_hand$pitcher_id,
        FUN = simulate_pitch_score,
        n = 10000,
        context = data_filtered,
        pitch_distrib_model = pitch_distrib_model,
        pitch_outcome_model = pitch_outcome_model
      ))
      pred_model <- rbind(pred_model,new_pred_model)
    }
  }
  return(pred_model)
}


pred_sim_compilation_full_year<-function(year, pitch_outcome_model, verbose=TRUE){
  pred_model <- data.frame("pitcher_id"=numeric(0), "pitch_type"=character(0),
                           "even"=logical(0), "count"=numeric(0), "mean_prob_swing"=numeric(0), "mean_prob_hbp"=numeric(0),
                           "mean_prob_strike"=numeric(0),"mean_prob_contact"=numeric(0),"mean_prob_fair"=numeric(0),
                           "mean_pred_hit"=numeric(0),"mean_pitch_value"=numeric(0),"mean_pred_value"=numeric(0))
  pitch <- readr::read_csv(paste("predictive-pitch-score/data/pitch/",year,".csv",sep=""))
  event <- readr::read_csv(paste("predictive-pitch-score/data/event/",year,".csv",sep=""))
  for(pitch_index in c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS")){
    data <- pitch |>
      dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
      dplyr::filter(pitch_type == pitch_index,
                    pre_balls < 4,
                    pre_strikes < 3
      )
    data$is_rhb <- data |>
      with(bat_side == "R") |>
      as.numeric()
      if(verbose==TRUE){
        print(paste("Fitting:",pitch_index,"Time:",Sys.time()))
      }
      pitch_distrib_model <- readRDS(glue::glue("~/Downloads/distribution/{pitch_index}/{year}.rds"))
      data_filtered <- data |>
        dplyr::filter(pitcher_id %in% pitch_distrib_model$pitcher_hand$pitcher_id)
      num_pitchers <- length(pitch_distrib_model$pitcher_hand$pitcher_id)
      new_pred_model <- data.frame("pitcher_id"=numeric(num_pitchers),
                                   "pitch_type"=character(num_pitchers),
                                   "even"=logical(num_pitchers),
                                   "count"=numeric(num_pitchers),
                                   "mean_prob_swing"=numeric(num_pitchers),
                                   "mean_prob_hbp"=numeric(num_pitchers),
                                   "mean_prob_strike"=numeric(num_pitchers),
                                   "mean_prob_contact"=numeric(num_pitchers),
                                   "mean_prob_fair"=numeric(num_pitchers),
                                   "mean_pred_hit"=numeric(num_pitchers),
                                   "mean_pitch_value"=numeric(num_pitchers),
                                   "mean_pred_value"=numeric(num_pitchers))
      new_pred_model[,1] <- pitch_distrib_model$pitcher_hand$pitcher_id
      new_pred_model[,2] <- pitch_index
      new_pred_model[,3] <- "full_season"
      new_pred_model[,4] <- NA
      new_pred_model[,5:12] <- t(sapply(
        X = pitch_distrib_model$pitcher_hand$pitcher_id,
        FUN = simulate_pitch_score,
        n = 10000,
        context = data_filtered,
        pitch_distrib_model = pitch_distrib_model,
        pitch_outcome_model = pitch_outcome_model
      ))
      pred_model <- rbind(pred_model,new_pred_model)
  }
  return(pred_model)
}

