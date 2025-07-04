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

testthat::test_that("regexes and section heads are in harmony", {

  for (has_rdname in c(TRUE, FALSE)) {
    section_heads <- pkg_doc_obj_section_heads__(
      regex = "mypkg::myfun",
      has_rdname = TRUE
    )
    regexes <- pkg_doc_obj_regex_set__(
      regex = "mypkg::myfun",
      type = "full"
    )
    testthat::expect_identical(names(section_heads), names(regexes))
  }
})
