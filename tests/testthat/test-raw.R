




testthat::test_that("extract_keyed_comment_blocks works", {

  test_script_path <- system.file("example_text_files/r_script.R",
                                  package = "codedoc")

  result <- extract_keyed_comment_blocks(
    text_file_path = test_script_path
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





