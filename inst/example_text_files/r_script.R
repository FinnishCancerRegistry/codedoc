


birth_date_to_current_age <- function(birth_date) {
  stopifnot(inherits(birth_date, "Date"))

  # @codedoc_comment_block current_age
  # current_age values are formed in function birth_date_to_current_age by
  # counting the number of days between today and the birth day.
  # @codedoc_comment_block current_age

  current_age_in_days <- as.numeric(Sys.Date() - birth_date)

  # @codedoc_comment_block current_age
  # the days are transformed into fractional years by dividing by 365.242199.
  # @codedoc_comment_block current_age

  current_age_in_days / 365.242199
}



age_to_age_group <- function(age) {

  # comment blocks can be nested
  # @codedoc_comment_block age_group_definitions
  #
  # **below is taken from age_group comment block**
  #
  # @codedoc_comment_block age_group
  # age_group values are formed in function **age_to_age_group**. The function
  # creates 5 age groups from age values by splitting
  # the age values at breaks `{0, 20, 40, 60, 80, Inf}`
  # @codedoc_comment_block age_group
  # @codedoc_comment_block age_group_definitions

  br <- c(0, 20, 40, 60, 80, Inf)
  cut(age, breaks = br, labels = FALSE, right = FALSE)
}


age_to_pension_age_indicator <- function(age) {

  # comment blocks can be nested
  # @codedoc_comment_block age_group_definitions
  #
  # **below is taken from pension_age_indicator comment block**
  #
  # @codedoc_comment_block pension_age_indicator
  # pension_age_indicator is created in function age_to_pension_age_indicator
  # by detecting whether age is over 65 or not.
  # ```{r, results = "asis"}
  # # I wrote this here only for demonstration purposes.
  # knitr::kable(data.frame(
  #   pension_age_indicator = c(FALSE, TRUE),
  #   text = c("Is too young to be pensioner", "Is of pension age")
  # ))
  # ```
  # @codedoc_comment_block pension_age_indicator
  # @codedoc_comment_block age_group_definitions

  age >= 65.0
}


