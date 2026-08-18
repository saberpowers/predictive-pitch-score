
# Parse command line options ----

opt_list <- list(
  optparse::make_option(c("-s", "--start_year"), type = "integer", default = 2022),
  optparse::make_option(c("-e", "--end_year"), type = "integer", default = 2022),
  optparse::make_option(c("-l", "--level"), type = "character", default = "mlb"),
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
    logger::log_info("Downloading {year} {opt$level}")
  }

  data <- sabRmetrics::download_statsapi(
    start_date = glue::glue("{year}-01-01"),
    end_date = glue::glue("{year}-12-31"),
    level = opt$level,
    cl = cluster
  )

  for (data_type in c("event", "pitch", "play", "game")) {
    dir <- file.path(opt$data_dir, data_type, opt$level)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(data[[data_type]], file = file.path(dir, paste0(year, ".csv")))
  }
}

if (opt$parallel) {
  parallel::stopCluster(cluster)
}
