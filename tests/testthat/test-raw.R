




testthat::test_that("extract_keyed_comment_blocks works", {

  result <- codedoc::extract_keyed_comment_blocks(
    text_file_path = codedoc::example_text_file_path("r_script.R"),
    detect_comment_lines = function(x) grepl("#", x),
    clean_comment_lines = function(x) sub("^[ #*-]*", "", x)
  )
  testthat::expect_true(is.data.frame(result))
  # NOTE: tests also that the first extracted block is the first one in the file
  # (as opposed to e.g. first one in alphabetical order of key)
  testthat::expect_true(
    result[["first_block_line"]][1L] == 8L
  )
  testthat::expect_true(
    result[["last_block_line"]][1L] == 9L
  )
  testthat::expect_true(
    all(result[["key"]][1:2] == "current_age")
  )

  result <- codedoc::extract_keyed_comment_blocks(
    text_file_path = codedoc::example_text_file_path("sql_script.sql"),
    detect_comment_lines = function(x) grepl("", x),
    clean_comment_lines = function(x) sub("^[ -/*]*", "", x)
  )
  testthat::expect_equal(
    result[["first_block_line"]], c(4L, 12L, 19L)
  )
  testthat::expect_equal(
    result[["last_block_line"]], c(6L, 15L, 22L)
  )
  testthat::expect_equal(
    result[["key"]], c("block_1", "block_2", "block_2")
  )
})







