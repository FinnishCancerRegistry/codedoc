




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
      value <- eval(expr, envir = env)
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







