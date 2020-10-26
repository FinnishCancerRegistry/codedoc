




testthat::test_that("extract_keyed_comment_blocks works", {

  result <- codedoc::extract_keyed_comment_blocks(
    text_file_path = codedoc::example_text_file_path("r_script.R")
  )

  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(
    result[["first_block_line"]][1L] == 8L
  )
  testthat::expect_true(
    result[["last_block_line"]][1L] == 10L
  )
  testthat::expect_true(
    result[["key"]][1L] == "current_age"
  )

})





