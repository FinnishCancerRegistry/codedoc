




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
  #   `${codedoc:::string_interpolation_regex()}`
  #
  # @codedoc_comment_block string_interpolation_regex
  "[$][{][^}]*[}]"
}

string_interpolation <- function(x, env, debug_data) {
  dbc::assert_prod_input_is_character_vector(x)
  dbc::assert_prod_input_is_environment(env)
  dbc::assert_prod_input_is_named_list(debug_data)
  dbc::assert_prod_input_has_names(
    debug_data, required_names = c("text_file_path", "first_line_no")
  )

  # @codedoc_comment_block codedoc:::string_interpolation
  #
  # @codedoc_insert_comment_block string_interpolation_regex
  #
  # @codedoc_comment_block codedoc:::string_interpolation
  ip_re <- string_interpolation_regex()
  ip_exprs <- unique(unlist(string_extract_all(x = x, pattern = ip_re)))
  if (length(ip_exprs) == 0L) {
    return(x)
  }
  # @codedoc_comment_block codedoc:::string_interpolation
  #
  # - each collected unique expression is evaluated using eval()
  #
  # @codedoc_comment_block codedoc:::string_interpolation
  ip_expr_values <- vapply(
    ip_exprs,
    function(expr) {
      expr <- sub(pattern = "^[$][{]", replacement = "", x = expr)
      expr <- sub(pattern = "[}]$", replacement = "", x = expr)
      expr <- parse(text = expr)[[1L]]
      # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-03-31", "0.3.7")
      # `[codedoc:extract_keyed_comment_blocks]` string interpolation improved.
      # If evaluating an expression results in an error, a more informative
      # error message is produced than previously. New error message includes
      # errored expression, original error message,
      # path to file producing the error and first line number of guilty
      # part of text.
      # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-03-31", "0.3.7")

      value <- tryCatch(
        eval(expr, envir = env),
        error = function(e) e
      )
      if (inherits(value, "error")) {
        stop("Error in ${} string interpolation: expression resulted in error.",
             " this was the expression: `", deparse1(expr), "`. ",
             "this was the error message: ", deparse1(value[["message"]]), ". ",
             "see file ", debug_data[["text_file_path"]],
             " starting from line no ", debug_data[["first_line_no"]])
      }
      # @codedoc_comment_block codedoc:::string_interpolation
      #
      # - if an expression produces an object of length more than one,
      #   separate elements are pasted together with one whitespace as a
      #   separator
      #
      # @codedoc_comment_block codedoc:::string_interpolation
      paste0(unlist(value), collapse = " ")
    },
    character(1L)
  )

  # @codedoc_comment_block codedoc:::string_interpolation
  #
  # - each expressions' results are inserted back into text using gsub
  #
  # @codedoc_comment_block codedoc:::string_interpolation
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







