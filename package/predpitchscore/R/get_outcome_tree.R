
get_outcome_tree <- function(call_code) {

  outcome_tree <- tibble::tibble(
    is_swing = call_code %in% c(
      "Foul", "Foul Bunt", "Foul Tip", "In play, no out", "In play, out(s)", "In play, run(s)",
      "Missed Bunt", "Swinging Strike", "Swinging Strike (Blocked)"
    ),
    is_hit_by_pitch = call_code %in% c("Hit by Pitch"),
    is_called_strike = call_code %in% c("Called Strike"),
    is_contact = call_code %in% c("Foul", "In play, no out", "In play, out(s)", "In play, run(s)"),
    is_fair = call_code %in% c("In play, no out", "In play, out(s)", "In play, run(s)")
  )
}
