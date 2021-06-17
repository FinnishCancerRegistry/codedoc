






testthat::test_that("string_interpolation works", {

  neo <- "mr. anderson"
  testthat::expect_identical(
    string_interpolation("hello ${neo}", environment()),
    "hello mr. anderson"
  )

})








