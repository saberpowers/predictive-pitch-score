#Work on plot more
#Add low_n
#Add RMSE for results
setwd("~/Documents/GitHub/predictive-pitch-score")
devtools::load_all("package/predpitchscore")
library(readr)
library(dplyr)
#Downloads Data and gets values ready
year <- 2022
pitch <- read_csv(paste("predictive-pitch-score/data/pitch/",year,".csv",sep=""))
event <- read_csv(paste("predictive-pitch-score/data/event/",year,".csv",sep=""))
pitch_outcome_model <- readRDS("~/Documents/GitHub/predictive-pitch-score/predictive-pitch-score/models/pitch_outcome_model.rds")
pitch_stuff_outcome_model <- readRDS("~/Documents/GitHub/predictive-pitch-score/predictive-pitch-score/models/stuff_model.rds")
base_out_run_exp <- read_csv("~/Documents/GitHub/predictive-pitch-score/predictive-pitch-score/models/base_out_run_exp.csv")
count_value <- read_csv("~/Documents/GitHub/predictive-pitch-score/predictive-pitch-score/models/count_value.csv")
pred_model <- readRDS(paste("pred_model_",year,".rds",sep=""))
pred_stuff_model <- readRDS(paste("pred_stuff_model_",year,".rds",sep=""))
cutoff <- 100

pitch$is_rhb <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  with(bat_side == "R") |>
  as.numeric()

pitch <- dplyr::filter(pitch,is.na(extension)!=1)

data <- pitch |>
  dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
  dplyr::filter(pitch_type %in% c("FF", "SI", "FC", "SL", "CU", "KC", "CH", "FS"))

#Does Descriptive
desc <- predict(pitch_outcome_model,newpitch = data)
data_with_desc <- cbind(data,desc)
data_with_desc <- dplyr::filter(data_with_desc,is.na(pitch_value)!=1)

desc_means_sds <- data_with_desc |>
  dplyr::group_by(pitcher_id,pitch_type,even=game_id %% 2==0) |>
  dplyr::summarize(count=n(),
                   mean_pitch_value=mean(pitch_value),
                   var_pitch_value=var(pitch_value))
desc_means_sds <- dplyr::filter(desc_means_sds,count>=cutoff)
desc_evens <- dplyr::filter(desc_means_sds,even=="TRUE")
desc_odds <- dplyr::filter(desc_means_sds,even=="FALSE")
regressed_desc_odds <- regressed_means_sds(desc_odds)
regressed_desc_evens <- regressed_means_sds(desc_evens)

#Does Stuff
newdata = data |>
  get_quadratic_coef() |>
  get_trackman_metrics() |>
  dplyr::select(dplyr::all_of(config_pitch_outcome_xgb$features_stuff)) |>
  as.matrix()

stuff <-predict(pitch_stuff_outcome_model,newdata)
data_with_stuff <- cbind(data,stuff)
data_with_stuff <- dplyr::filter(data_with_stuff,is.na(stuff)!=1)

stuff_means_sds <- data_with_stuff |>
  dplyr::group_by(pitcher_id,pitch_type,even=game_id %% 2==0) |>
  dplyr::summarize(count=n(),
                   mean_pitch_value=mean(stuff),
                   var_pitch_value=var(stuff))
stuff_means_sds <- dplyr::filter(stuff_means_sds,count>=cutoff)
stuff_evens <- dplyr::filter(stuff_means_sds,even=="TRUE")
stuff_odds <- dplyr::filter(stuff_means_sds,even=="FALSE")
regressed_stuff_odds <- regressed_means_sds(stuff_odds)
regressed_stuff_evens <- regressed_means_sds(stuff_evens)

#Does Predictive
pred_evens <- dplyr::filter(pred_model,even=="TRUE")[c(1:4,11)]
pred_odds <- dplyr::filter(pred_model,even=="FALSE")[c(1:4,11)]
full_pred_evens <- inner_join(pred_evens,desc_evens,
                              by=c("pitcher_id","pitch_type","even"))
full_pred_evens$var_pitch_value=full_pred_evens$var_pitch_value*0.5
noise_variance_evens <- full_pred_evens$var_pitch_value/full_pred_evens$count.y
population_mean_evens <- weighted.mean(full_pred_evens$mean_pitch_value.x-
                                         full_pred_evens$mean_pitch_value.y,1/noise_variance_evens)
pred_evens$mean_pitch_value<-pred_evens$mean_pitch_value-population_mean_evens
full_pred_odds <- inner_join(pred_odds,desc_odds,
                             by=c("pitcher_id","pitch_type","even"))
full_pred_odds$var_pitch_value=full_pred_odds$var_pitch_value*0.5
noise_variance_odds <- full_pred_odds$var_pitch_value/full_pred_odds$count.y
population_mean_odds <- weighted.mean(full_pred_odds$mean_pitch_value.x-
                                        full_pred_odds$mean_pitch_value.y,1/noise_variance_odds)
pred_odds$mean_pitch_value<-pred_odds$mean_pitch_value-population_mean_odds

full_pred_evens<-full_pred_evens[,c(1:5,8)]
full_pred_odds<-full_pred_odds[,c(1:5,8)]
colnames(full_pred_evens)<-colnames(desc_evens)
colnames(full_pred_odds)<-colnames(desc_odds)

#Does Predictive Stuff
pred_stuff_evens <- dplyr::filter(pred_stuff_model,even=="TRUE")
pred_stuff_odds <- dplyr::filter(pred_stuff_model,even=="FALSE")

#Does Stuff+Descriptive
regressed_desc_minus_stuff_odds <- regressed_means_sds_diff(desc_odds,stuff_odds)
regressed_desc_minus_stuff_evens <- regressed_means_sds_diff(desc_evens,stuff_evens)

desc_with_stuff_odds <- inner_join(regressed_desc_minus_stuff_odds,
                                   regressed_stuff_odds,by=c("pitcher_id","pitch_type","even"))
desc_with_stuff_odds$mean_pitch_value.x <- desc_with_stuff_odds$mean_pitch_value.x+
  desc_with_stuff_odds$mean_pitch_value.y
desc_with_stuff_odds <- desc_with_stuff_odds[,1:5]
colnames(desc_with_stuff_odds)<-colnames(desc_odds)[1:5]
desc_with_stuff_evens <- inner_join(regressed_desc_minus_stuff_evens,
                                   regressed_stuff_evens,by=c("pitcher_id","pitch_type","even"))
desc_with_stuff_evens$mean_pitch_value.x <- desc_with_stuff_evens$mean_pitch_value.x+desc_with_stuff_evens$mean_pitch_value.y
desc_with_stuff_evens <- desc_with_stuff_evens[,1:5]
colnames(desc_with_stuff_evens)<-colnames(desc_evens)[1:5]

#Desc with pred
regressed_desc_minus_pred_odds <- regressed_means_sds_diff(desc_odds,full_pred_odds)
regressed_desc_minus_pred_evens <- regressed_means_sds_diff(desc_evens,full_pred_evens)

desc_with_pred_odds <- inner_join(regressed_desc_minus_pred_odds,
                                   pred_odds,by=c("pitcher_id","pitch_type","even"))
desc_with_pred_odds$mean_pitch_value.x <- desc_with_pred_odds$mean_pitch_value.x+
  desc_with_pred_odds$mean_pitch_value.y
desc_with_pred_odds <- desc_with_pred_odds[,1:5]
colnames(desc_with_pred_odds)<-colnames(desc_odds)[1:5]
desc_with_pred_evens <- inner_join(regressed_desc_minus_pred_evens,
                                    pred_evens,by=c("pitcher_id","pitch_type","even"))
desc_with_pred_evens$mean_pitch_value.x <- desc_with_pred_evens$mean_pitch_value.x+
  desc_with_pred_evens$mean_pitch_value.y
desc_with_pred_evens <- desc_with_pred_evens[,1:5]
colnames(desc_with_pred_evens)<-colnames(desc_evens)[1:5]

scores_df <- data.frame("comparison"=character(0),"pitch_value"=numeric(0))
scores_df[1,1] <- "Regressed Desc vs desc"
scores_df[1,2] <- (RMSE_scores(regressed_desc_evens,desc_odds)+
                    RMSE_scores(regressed_desc_odds,desc_evens))/2
scores_df[2,1] <- "Regressed Stuff vs desc"
scores_df[2,2] <- (RMSE_scores(regressed_stuff_evens,desc_odds)+
                    RMSE_scores(regressed_stuff_odds,desc_evens))/2
scores_df[3,1] <- "Predictive vs desc"
scores_df[3,2] <- (RMSE_scores(pred_evens,desc_odds)+
                    RMSE_scores(pred_odds,desc_evens))/2
scores_df[4,1] <- "Predictive Stuff vs desc"
scores_df[4,2] <- (RMSE_scores(pred_stuff_evens,desc_odds)+
                       RMSE_scores(pred_stuff_odds,desc_evens))/2
scores_df[5,1] <- "Desc with Stuff vs desc"
scores_df[5,2] <- (RMSE_scores(desc_with_stuff_evens,desc_odds)+
                       RMSE_scores(desc_with_stuff_odds,desc_evens))/2
scores_df[6,1] <- "Desc with Pred vs desc"
scores_df[6,2] <- (RMSE_scores(desc_with_pred_evens,desc_odds)+
                       RMSE_scores(desc_with_pred_odds,desc_evens))/2
scores_df[7,1] <- "Unregressed desc vs desc"
scores_df[7,2] <- (RMSE_scores(desc_evens,desc_odds)+
                    RMSE_scores(desc_odds,desc_evens))/2
scores_df[8,1] <- "Unregressed Stuff vs desc"
scores_df[8,2] <- (RMSE_scores(stuff_evens,desc_odds)+
                    RMSE_scores(stuff_odds,desc_evens))/2
scores_df[,2] <- round(scores_df[,2],6)*2000


combined <- inner_join(desc_evens,desc_odds,by=c("pitcher_id","pitch_type"))
mean_sampling_variance <- weighted.mean(c(combined$var_pitch_value.x,
                                          combined$var_pitch_value.y),
                                        w=c(combined$count.x,
                                            combined$count.y))
plot(seq(100,1000),2000*(mean_sampling_variance/seq(100,1000))^0.5,type="l",col="red",
     xlab="Count",ylab="RMSE per 2000 pitches",main="Error Comparison",ylim=c(0,300*(mean_sampling_variance)^0.5))
combine_even_odds(pred_odds,pred_evens,"blue")
#combine_even_odds(regressed_stuff_odds,regressed_stuff_evens,"green")
combine_even_odds(regressed_desc_odds,regressed_desc_evens,"purple")
combine_even_odds(desc_odds,desc_evens,"green")
lines(seq(100,1000),2000*(2*mean_sampling_variance/seq(100,1000))^0.5,col="brown")

legend("topright", legend = c("Sampling Variance", "Predictive","Regressed Descriptive","Unregressed Descriptive",
                              "Sampling Variance*2"),
       lwd = 1, col = c("red", "blue","purple","green","brown"))

combine_even_odds<-function(means_sds_odds,means_sds_evens,color){
  combined_1<-inner_join(means_sds_odds,desc_evens,by=c("pitcher_id","pitch_type"))
  combined_2<-inner_join(means_sds_evens,desc_odds,by=c("pitcher_id","pitch_type"))
  combined<-rbind(combined_1,combined_2)
  combined$squared_error<-(combined$mean_pitch_value.x-combined$mean_pitch_value.y)^2
  #spline_regression<-lm(squared_error~splines::ns(count.y,df=5),data=combined)
  #expected_error<-predict(spline_regression,list(count.y=seq(100,1000)))
  loess_regression<-loess(squared_error~count.y,data=combined)
  expected_error<-predict(loess_regression,data.frame(count.y=seq(100,1000)),se=TRUE)
  lines(seq(100,1000),2000*expected_error$fit^0.5,col=color)
  lines(seq(100,1000),2000*(expected_error$fit+expected_error$se.fit*1.96)^0.5,col=color,lty=2)
  lines(seq(100,1000),2000*(expected_error$fit-expected_error$se.fit*1.96)^0.5,col=color,lty=2)
  cutoffs<-quantile(combined$count.y,seq(0,1,0.05))
  cutoffs[length(cutoffs)]<-Inf
  x<-cutoffs[seq(2,20,2)]
  y<-c()
  for(i in 1:10){
    y[i]<-2000*(RMSE_scores(means_sds_odds,desc_evens,cutoffs[seq(1,19,2)][i],cutoffs[seq(3,21,2)][i])+
                  RMSE_scores(means_sds_evens,desc_odds,cutoffs[seq(1,19,2)][i],cutoffs[seq(3,21,2)][i]))/2
  }
  points(x,y,col=color)
}

RMSE_scores <- function(means1,means2,min_n=0,max_n=Inf){
  means1 <- means1[,1:5]
  means2 <- means2[,1:5]
  colnames(means1)[4:5] <- paste(colnames(means1)[4:5],"_1",sep="")
  colnames(means2)[4:5] <- paste(colnames(means2)[4:5],"_2",sep="")
  combined <- inner_join(means1,means2,by=c("pitcher_id","pitch_type"))
  combined <- dplyr::filter(combined,count_2>=min_n,count_2<max_n)
  pitch_value <- mean(combined$count_2*(combined$mean_pitch_value_1-combined$mean_pitch_value_2)^2)^0.5/mean(combined$count_2)^0.5
  return(c(pitch_value))
}


regressed_means_sds <- function(means_sds,verbose=TRUE,max_iterations = 1e4,
                                tolerance = 1e-7 * var(means_sds$mean_pitch_value)){
  adjusted_means_sds <- means_sds[,1:5]
  mean_sampling_variance <- weighted.mean(means_sds$var_pitch_value,means_sds$count)
  noise_variance <- means_sds$var_pitch_value/means_sds$count
  population_mean <- weighted.mean(means_sds$mean_pitch_value,1/noise_variance)
  point_estimate = (means_sds$mean_pitch_value - population_mean)^2 - noise_variance
  last_population_variance = 0
  population_variance = mean((means_sds$mean_pitch_value - population_mean)^2)
  t = 0
  while(abs(population_variance - last_population_variance) > tolerance & t < max_iterations) {
    t = t + 1
    last_population_variance = population_variance
    weight = (noise_variance + population_variance)^{-2}
    population_variance = weighted.mean(point_estimate, w = weight)
  }
  ballast <- min(mean_sampling_variance/population_variance,2000)
  if(verbose==TRUE){
    print(ballast)
  }
  adjusted_means_sds$mean_pitch_value <- (means_sds$mean_pitch_value*means_sds$count+
                                            population_mean*ballast)/(means_sds$count+ballast)
  return(adjusted_means_sds)
}

regressed_means_sds_diff <- function(means_sds_1,means_sds_2,verbose=TRUE,max_iterations = 1e4,
                                     tolerance = 1e-7 * var(means_sds_1$mean_pitch_value)){
  adjusted_means_sds_diff <- means_sds_1[,1:5]
  combined <- inner_join(means_sds_1,means_sds_2,by=c("pitcher_id","pitch_type"))
  means_sds_diff <- combined$mean_pitch_value.x-combined$mean_pitch_value.y
  mean_sampling_variance <- weighted.mean(combined$var_pitch_value.x-
                                            combined$var_pitch_value.y,combined$count.x)
  noise_variance <- (combined$var_pitch_value.x-
                       combined$var_pitch_value.y)/combined$count.x
  noise_variance <- pmax(noise_variance,mean_sampling_variance/means_sds_1$count*0.25) 
  #Makes sure that there are no negative numbers or points that are weighted too highly
  #This is because stuff variance technically could be higher than descriptive
  population_mean <- weighted.mean(means_sds_diff,1/noise_variance)
  point_estimate = (means_sds_diff - population_mean)^2 - noise_variance
  last_population_variance = 0
  population_variance = mean((means_sds_diff - population_mean)^2)
  t = 0
  while(abs(population_variance - last_population_variance) > tolerance & t < max_iterations) {
    t = t + 1
    last_population_variance = population_variance
    weight = (noise_variance + population_variance)^{-2}
    population_variance = weighted.mean(point_estimate, w = weight)
  }
  ballast <- min(mean_sampling_variance/population_variance,2000)
  if(verbose==TRUE){
    print(ballast)
  }
  adjusted_means_sds_diff$mean_pitch_value <- (means_sds_diff*combined$count.x+
                                                 population_mean*ballast)/(combined$count.x+ballast)
  return(adjusted_means_sds_diff)
}

get_trackman_metrics <- function(data) {
  
  trackman_metrics <- data |>
    dplyr::mutate(
      
      release_x = cx,
      release_y = cy,
      release_z = cz,
      
      # Calculate plate location
      plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
      # Solve quadratic equation to get the time at which ball reaches front of plate
      plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
      plate_x = ax * plate_time^2 / 2 + bx * plate_time + cx,
      plate_z = az * plate_time^2 / 2 + bz * plate_time + cz,
      
      # Set up some intermediate variables for calculating breaks
      gravity = -32.17,   # feet per second per second
      plate_x_line = bx * plate_time + cx,
      plate_z_line = bz * plate_time + cz,
      plate_z_gravity = gravity * plate_time^2 / 2 + bz * plate_time + cz,
      
      # SP: I'm reconstructing these from memory, so not 100% sure they're correct
      horz_break = 12 * (plate_x - plate_x_line),               # measured in inches
      vert_break = 12 * (plate_z - plate_z_line),               # measured in inches
      induced_vert_break = 12 * (plate_z - plate_z_gravity),    # measured in inches
      
      # Also recover the TrackMan metrics necessary for calculating the quadratic coefficients
      # t0 is the time when y = 50 (necessary for calculating velocity and x/z location at time t0)
      t0 = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - 50))) / (2 * (ay / 2)),
      vx0 = ax * t0 + bx,
      vy0 = ay * t0 + by,
      vz0 = az * t0 + bz,
      x0 = ax * t0^2 / 2 + bx * t0 + cx,
      z0 = az * t0^2 / 2 + bz * t0 + cz,
      extension = 60.5 - cy,
      release_speed = sqrt(bx^2 + by^2 + bz^2)
    )
  
  return(trackman_metrics)
}

config_pitch_outcome_xgb <- list(
  
  features_context = c("pre_balls", "pre_strikes", "is_rhb", "strike_zone_top", "strike_zone_bottom"),
  features_pitch = c("plate_x", "plate_z", "plate_vx", "plate_vy", "plate_vz", "ax", "ay", "az", "extension"),
  features_stuff = c("release_x", "release_y", "release_z", "release_speed", "induced_vert_break", "horz_break"),
  
  nrounds_swing = 1500,
  params_swing = list(eta = 0.05, gamma = 0, max_depth = 9, min_child_weight = 10, subsample = 0.65, colsample_bytree = 0.7),
  
  nrounds_hbp = 1000,
  params_hbp = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),
  
  nrounds_strike = 2000,
  params_strike = list(eta = 0.01, gamma = 0, max_depth = 9, min_child_weight = 10, subsample = 0.65, colsample_bytree = 0.7),
  
  nrounds_contact = 1000,
  params_contact = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),
  
  nrounds_fair = 1500,
  params_fair = list(eta = 0.01, gamma = 0, max_depth = 9, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),
  
  nrounds_hit = 1000,
  params_hit = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),
  
  nrounds_stuff = 1000,
  params_stuff = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),
  
  nrounds_value = 2000,
  params_value = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),
  
  # Maximum nrounds for tuning
  nrounds_max = 2000,
  
  # List of parameter combinations to try for tuning
  params_list = expand.grid(
    nthread = 1,  # turn off xgb threading (threading across parameter sets is more efficient)
    eta = c(0.01, 0.05, 0.3),
    gamma = 0,
    max_depth = c(3, 6, 9),
    min_child_weight = c(10, 30, 100),
    subsample = 0.65,
    colsample_bytree = 0.7
  ) |>
    dplyr::arrange(-max_depth, min_child_weight) |>   # work on longest training time first
    apply(MARGIN = 1, FUN = as.list)
)
