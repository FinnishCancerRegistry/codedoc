testthat::test_that("string_interpolation works", {

  neo <- "mr. anderson"
  obs <- string_interpolation(
    "hello ${neo}",
    environment(),
    debug_data = list(
      text_file_path = "[unknown text path]",
      first_line_no = 1L
    )
  )
  exp <- "hello mr. anderson"
  testthat::expect_identical(obs, exp)
})
