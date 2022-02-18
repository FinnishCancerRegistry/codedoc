

#' @title `codedoc` Comment Block Lines
#' @description
#' Tools to extract comment blocks and return a vector of strings (lines).
#' @param detect_allowed_keys see `[codedoc::extract_keyed_comment_blocks]`
#' @param text_file_paths see `[codedoc::extract_keyed_comment_blocks]`
#' @param extract_arg_list
#'
#' - `NULL`: no additional args
#' - `list`: these args are passed to `[codedoc::extract_keyed_comment_blocks]`
#' @template arg_assertion_type
#' @name codedoc_lines
NULL

#' @rdname codedoc_lines
#' @export
codedoc_lines <- function(
  detect_allowed_keys,
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::codedoc_lines", "2022-02-17", "0.2.15")
  # New exported function `[codedoc::codedoc_lines]`.
  # @codedoc_comment_block news("codedoc::codedoc_lines", "2022-02-17", "0.2.15")
  dbc::assert_is_one_of(
    extract_arg_list,
    funs = list(
      dbc::report_is_NULL,
      dbc::report_is_list
    ),
    assertion_type = assertion_type
  )
  assert_arg_text_file_paths(text_file_paths, assertion_type = assertion_type)
  extract_arg_list <- as.list(extract_arg_list)
  extract_arg_list[["detect_allowed_keys"]] <- detect_allowed_keys
  extract_arg_list[["text_file_paths"]] <- text_file_paths
  df <- do.call(codedoc::extract_keyed_comment_blocks, extract_arg_list,
                quote = TRUE)
  unlist(lapply(df[["comment_block"]], function(x) {
    c("", x, "")
  }))
}
