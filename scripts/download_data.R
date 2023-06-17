
library(predpitchscore)

year <- 2016
data_dir <- "data"
extract_season(year = year, data_dir = data_dir)
consolidate_data(year = year, data_dir = data_dir)
