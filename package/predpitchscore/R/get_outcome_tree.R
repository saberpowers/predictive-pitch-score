
get_outcome_tree <- function(call_code) {

  outcome_tree <- tibble::tibble(
    is_swing = call_code %in% c("D", "E", "F", "S", "T", "W", "X"),
    is_hit_by_pitch = call_code %in% c("H"),
    is_called_strike = call_code %in% c("C"),
    is_contact = call_code %in% c("D", "E", "F", "X"),
    is_fair = call_code %in% c("D", "E", "X")
  )
}
