




string_extract_first <- function(x, pattern, ...) {
  m <- regexpr(text = x, pattern = pattern, ...)
  regmatches(x = x, m = m)
}
string_extract_all <- function(x, pattern, ...) {
  m <- gregexpr(text = x, pattern = pattern, ...)
  regmatches(x = x, m = m)
}


string_interpolation_regex <- function() {
  # @codedoc_comment_block string_interpolation_regex
  #
  # - interpolation expressions are detected using regex
  #   `${string_interpolation_regex()}`
  #
  # @codedoc_comment_block string_interpolation_regex
  "[$][{][^}]*[}]"
}

string_interpolation <- function(x, env) {
  dbc::assert_prod_input_is_character_vector(x)
  dbc::assert_prod_input_is_environment(env)

  # @codedoc_comment_block string_interpolation_details
  #
  # @codedoc_insert_comment_block string_interpolation_regex
  #
  # @codedoc_comment_block string_interpolation_details
  ip_re <- string_interpolation_regex()
  ip_exprs <- unique(unlist(string_extract_all(x = x, pattern = ip_re)))
  if (length(ip_exprs) == 0L) {
    return(x)
  }
  # @codedoc_comment_block string_interpolation_details
  #
  # - each collected unique expression is evaluated using eval()
  #
  # @codedoc_comment_block string_interpolation_details
  ip_expr_values <- vapply(
    ip_exprs,
    function(expr) {
      expr <- sub(pattern = "^[$][{]", replacement = "", x = expr)
      expr <- sub(pattern = "[}]$", replacement = "", x = expr)
      expr <- parse(text = expr)[[1L]]
      value <- eval(expr, envir = env)
      # @codedoc_comment_block string_interpolation_details
      #
      # - if an expression produces an object of length more than one,
      #   separate elements are pasted together with one whitespace as a
      #   separator
      #
      # @codedoc_comment_block string_interpolation_details
      paste0(unlist(value), collapse = " ")
    },
    character(1L)
  )

  # @codedoc_comment_block string_interpolation_details
  #
  # - each expressions' results are inserted back into text using gsub
  #
  # @codedoc_comment_block string_interpolation_details
  for (i in 1:length(ip_exprs)) {
    x <- gsub(
      pattern = ip_exprs[i],
      replacement = ip_expr_values[i],
      x = x,
      fixed = TRUE
    )
  }
  return(x)
}
string_interpolation_details <- function() {
  block_df <- codedoc::extract_keyed_comment_blocks_(
    text_file_paths = c("R/utils.R", "R/collect_raw.R")
  )
  key <- "string_interpolation_details"
  block_df <- block_df[block_df[["key"]] == key, ]
  c(
    "@details",
    "Simple string interpolation is implemented ",
    "as follows:",
    "",
    unlist(block_df[["comment_block"]])
  )
}




full_call <- function(
  matched_call = eval(quote(match.call()), parent.frame(1L)),
  fun_symbol = matched_call[[1]]
) {
  fun <- eval(fun_symbol)
  full_call <- matched_call
  formals <- formals(fun)
  full_call[names(formals)] <- formals
  matched_call_arg_nms <- setdiff(names(matched_call), "")
  full_call[matched_call_arg_nms] <- matched_call[matched_call_arg_nms]
  full_call[[1L]] <- fun_symbol
  return(full_call)
}







