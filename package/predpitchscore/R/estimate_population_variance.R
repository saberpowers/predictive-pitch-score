#' Estimate population variance
#' 
#' Estimate the population variance in true talent for a performance metric given a vector of
#' observed values and associated noise variances.
#' 
#' @param observed_value vector of observed performance
#' @param noise_variance vector of estimated noise variance
#' @param population_mean scalar, population mean for `observed_value`
#' @param max_iterations integer, optimization stops after `max_iterations` is reached
#' @param tolerance scalar, optimization stops after movement in estimated population variance is
#'   less than tolerance
#' 
#' @return a scalar estimate of population variance
#' 
#' @export
#' 
estimate_population_variance <- function(observed_value,
                                         noise_variance,
                                         population_mean = weighted.mean(observed_value, w = 1 / noise_variance, na.rm = TRUE),
                                         max_iterations = 1e4,
                                         tolerance = 1e-7 * var(observed_value)) {

  point_estimate <- (observed_value - population_mean)^2 - noise_variance
  last_population_variance <- 0
  population_variance <- mean((observed_value - population_mean)^2)

  t <- 0
  while(abs(population_variance - last_population_variance) > tolerance & t < max_iterations) {

    t <- t + 1
    last_population_variance <- population_variance
    weight <- (noise_variance + population_variance)^{-2}
    population_variance <- weighted.mean(point_estimate, w = weight)
  }

  return(population_variance)
}