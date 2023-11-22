
# Parse command line options ----

opt_list <- list(
  optparse::make_option(c("-s", "--start_year"), type = "integer", default = 2022),
  optparse::make_option(c("-e", "--end_year"), type = "integer", default = 2022),
  optparse::make_option(c("-d", "--data_dir"), type = "character", default = "data"),
  optparse::make_option(c("-p", "--parallel"), action = "store_true", default = FALSE),
  optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE)
)
opt <- optparse::OptionParser(option_list = opt_list) |>
  optparse::parse_args()


# Download data ----

cluster <- NULL
if (opt$parallel) {
  cluster <- parallel::makeCluster(parallel::detectCores())
}

for (year in seq(from = opt$start_year, to = opt$end_year)) {

  if (opt$verbose) {
    logger::log_info("Downloading {year}")
  }

  data <- predpitchscore::extract_season(year = year, cl = cluster)
  write.csv(data$event, file = glue::glue("{opt$data_dir}/event/{year}.csv"), row.names = FALSE)
  write.csv(data$pitch, file = glue::glue("{opt$data_dir}/pitch/{year}.csv"), row.names = FALSE)
  write.csv(data$play, file = glue::glue("{opt$data_dir}/play/{year}.csv"), row.names = FALSE)
  write.csv(data$game, file = glue::glue("{opt$data_dir}/game/{year}.csv"), row.names = FALSE)
}

if (opt$parallel) {
  parallel::stopCluster(cluster)
}
