




testthat::test_that("read_raw_codedocs works", {

  test_script_path <- system.file("example_text_files/r_script.R",
                                  package = "codedoc")

  result <- read_raw_codedocs(
    text_file_path = test_script_path
  )

  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(
    result[["first_block_line"]][1L] == 8L
  )
  testthat::expect_true(
    result[["last_block_line"]][1L] == 9L
  )
  testthat::expect_true(
    result[["key"]][1L] == "age_group"
  )

})





