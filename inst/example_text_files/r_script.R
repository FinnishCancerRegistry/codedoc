



age_to_age_group <- function(age) {

  # @doc age_group
  # age_to_age_group creates 5 age groups from age values by splitting
  # the age values at breaks {0, 20, 40, 60, 80, Inf}
  # @doc age_group

  br <- c(0, 20, 40, 60, 80, Inf)
  cut(age, breaks = br, labels = FALSE, right = FALSE)
}



