#' Get outcome tree
#' 
#' Get binary outcomes (swing, hbp, strike, contact, fair) from pitch outcome description.
#' 
#' @param description character vector, e.g. "In play, out(s)"
#' 
#' @return dataframe with logical columns `is_swing`, `is_hbp`, `is_strike`, `is_contact`, `is_fair`
#' 
get_outcome_tree <- function(description) {

  outcome_tree <- tibble::tibble(
    is_swing = description %in% c(
      "Foul", "Foul Bunt", "Foul Tip", "In play, no out", "In play, out(s)", "In play, run(s)",
      "Missed Bunt", "Swinging Strike", "Swinging Strike (Blocked)"
    ),
    is_hbp = description %in% c("Hit by Pitch"),
    is_strike = description %in% c("Called Strike"),
    is_contact = description %in% c("Foul", "In play, no out", "In play, out(s)", "In play, run(s)"),
    is_fair = description %in% c("In play, no out", "In play, out(s)", "In play, run(s)")
  )

  return(outcome_tree)
}
