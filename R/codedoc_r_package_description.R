

#' @title `codedoc` to R Package Description
#' @description
#' Extract `codedoc` comment block(s) and turn them into R package
#' description help page (roxygen) and a `README.md`.
#' @name codedoc_R_package_description
#' @eval c(
#'   codedoc::codedoc_lines("^codedoc::codedoc_R_package_description__.*$")
#' )
NULL

#' @rdname codedoc_R_package_description
#' @export
codedoc_R_package_description <- function(
  R_package_name,
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
) {
  dbc::assert_is_character_nonNA_atom(
    R_package_name, assertion_type = assertion_type
  )

  # @codedoc_comment_block codedoc::codedoc_R_package_description__::R_package_name
  # @param R_package_name `[character]` (no default)
  #
  # Turned into `detect_allowed_keys` arg for `[codedoc::codedoc_lines]` via
  # ```r
  # detect_allowed_keys <- paste0(
  #   "^\\QR_package_description(", R_package_name, ")\\E$"
  # )
  # ```
  # @codedoc_comment_block codedoc::codedoc_R_package_description__::R_package_name
  detect_allowed_keys <- paste0(
    "^\\QR_package_description(", R_package_name, ")\\E$"
  )
  # @codedoc_comment_block codedoc::codedoc_R_package_description__::R_package_name
  # @param text_file_paths `[character]` see `[codedoc::codedoc_lines]`
  # @codedoc_comment_block codedoc::codedoc_R_package_description__::R_package_name

  # @codedoc_comment_block codedoc::codedoc_R_package_description__::R_package_name
  # @param extract_arg_list `[character]` see `[codedoc::codedoc_lines]`
  # @codedoc_comment_block codedoc::codedoc_R_package_description__::R_package_name
  lines <- codedoc::codedoc_lines(
    detect_allowed_keys = detect_allowed_keys,
    text_file_paths = text_file_paths,
    extract_arg_list = extract_arg_list,
    assertion_type = assertion_type
  )
  md_lines <- c(
    "",
    paste0("# Package `", R_package_name, "`"),
    "",
    lines,
    ""
  )
  writeLines(md_lines, "README.md")
  # @codedoc_comment_block return(codedoc::codedoc_R_package_description__)
  # A vector of strings is returned, where the first line is always
  # `"@description"`.
  # @codedoc_comment_block return(codedoc::codedoc_R_package_description__)
  return(c(
    "@description",
    "",
    lines,
    ""
  ))
}


