




testthat::test_that("extract_keyed_comment_blocks works", {

  result <- codedoc::extract_keyed_comment_blocks(
    text_file_path = codedoc::example_text_file_path("r_script.R")
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

  script_paths <- dir(
    system.file(
      "example_text_files",
      package = "codedoc"
    ),
    full.names = TRUE
  )
  script_paths <- fs_file_path_normalise(script_paths)
  names(script_paths) <- fs_file_type(script_paths)
  script_paths <- script_paths[names(script_paths) != "r"]
  result_by_file_type <- lapply(script_paths, function(sp) {
    codedoc::extract_keyed_comment_blocks(
      text_file_path = sp
    )
  })
  names(result_by_file_type) <- names(script_paths)

  for (file_type in names(script_paths)) {
    result <- result_by_file_type[[file_type]]
    testthat::expect_equal(
      result[["first_block_line"]], c(2L, 8L, 15L)
    )
    testthat::expect_equal(
      result[["last_block_line"]], c(4L, 11L, 18L)
    )
    testthat::expect_equal(
      result[["key"]], c("block_1", "block_2", "block_2")
    )
  }
})
