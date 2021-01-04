




codedoc_key_line_regex <- function() "(@codedoc_comment_block)"
detect_codedoc_key_lines <- function(x) {
  grepl(codedoc_key_line_regex(), x)
}





#' @title Read codedoc-formatted Code Comments
#' @description
#' Extracts blocks of specifically formatted comments from text file.
#' @param text_file_paths `[character]` (mandatory, no default)
#'
#' path to a text file; the file is not inspected in any way to be a text file
#' based on file extension or any other characteristics, it is merely assumed
#' to be one
#' @param detect_comment_lines `[function]`
#' (optional, default `function(x) grepl("^\\s*[#*]", x)`)
#'
#' a function which takes lines of text as input and outputs a logical vector
#' of the same length as input which is `TRUE` when the line is a comment line;
#' the default detects lines that start with the regex `"^\\s*[#*]\\s*"`
#' @param clean_comment_lines `[function]`
#' (optional, default `function(x) sub("^\\s*[#*]\\s*", "", x)`)
#'
#' a function which takes lines of text as input and outputs a character vector
#' of the same length as input; this function takes comment lines and should
#' strip any comment characters preceding the text itself;
#' the default removes any substrings matching regex `"^\\s*[#*]\\s*"`
#' @param readLines_arg_list `[list]` (mandatory, default `list()`)
#'
#' list of arguments passed to [readLines]; `con` is always set to
#' an element of `text_file_paths`
#' @details
#' `extract_keyed_comment_blocks` is intended to be used by the user and not
#' in other functions but is otherwise identical to
#' `extract_keyed_comment_blocks_`. `extract_keyed_comment_blocks_` is intended
#' to be used within other functions.
#' See `help(topic = "dbc", package = "dbc")` for discussion on this
#' distinction.
#' @examples
#'
#' block_df <- codedoc::extract_keyed_comment_blocks_(
#'   text_file_paths = codedoc::example_text_file_path("r_script.R")
#' )
#' print(block_df)
#'
#' @name extract_keyed_comment_blocks
NULL

extract_keyed_comment_blocks_assertions <- function(
  text_file_paths,
  detect_comment_lines,
  clean_comment_lines,
  readLines_arg_list,
  assertion_type
) {
  report_df <- eval(
    quote(
      rbind(
        dbc::report_is_character_nonNA_vector(text_file_paths),
        dbc::report_file_exists(text_file_paths),
        dbc::report_is_function_with_required_argument_names(
          detect_comment_lines,
          required_argument_names = "x"
        ),
        dbc::report_is_function_with_required_argument_names(
          clean_comment_lines,
          required_argument_names = "x"
        ),
        dbc::report_is_list(readLines_arg_list)
      )
    ),
    envir = parent.frame(1L)
  )

  dbc::report_to_assertion(
    report_df,
    assertion_type = assertion_type
  )
}

#' @rdname extract_keyed_comment_blocks
#' @export
extract_keyed_comment_blocks <- function(
  text_file_paths,
  detect_comment_lines = function(x) grepl("^\\s*[#*]\\s*", x),
  clean_comment_lines = function(x) sub("^\\s*[#*]\\s*", "", x),
  readLines_arg_list = list()
) {
  extract_keyed_comment_blocks_assertions(
    text_file_paths,
    detect_comment_lines,
    clean_comment_lines,
    readLines_arg_list,
    assertion_type = "user_input"
  )

  this_call <- match.call()
  this_call[[1L]] <- quote(extract_keyed_comment_blocks_)
  eval(this_call, envir = environment())
}

#' @rdname extract_keyed_comment_blocks
#' @export
extract_keyed_comment_blocks_ <- function(
  text_file_paths,
  detect_comment_lines = function(x) grepl("^\\s*[#*]\\s*", x),
  clean_comment_lines = function(x) sub("^\\s*[#*]\\s*", "", x),
  readLines_arg_list = list()
) {
  extract_keyed_comment_blocks_assertions(
    text_file_paths,
    detect_comment_lines,
    clean_comment_lines,
    readLines_arg_list,
    assertion_type = "prod_input"
  )

  empty_output_df <- data.frame(
    text_file_path = character(0),
    key = character(0),
    first_block_line = integer(0),
    last_block_line = integer(0),
    comment_block = vector("list", 0)
  )

  if (length(text_file_paths) > 1L) {
    arg_list <- mget(names(formals(extract_keyed_comment_blocks)))
    block_df_list <- lapply(text_file_paths, function(text_file_path) {
      arg_list[["text_file_paths"]] <- text_file_path
      do.call(extract_keyed_comment_blocks, arg_list)
    })
    block_df <- do.call(rbind, block_df_list)
    return(block_df)
  }

  readLines_arg_list[["con"]] <- text_file_paths

  line_df <- data.frame(
    line = do.call(readLines, readLines_arg_list)
  )
  line_df[["is_comment_line"]] <- detect_comment_lines(line_df[["line"]])
  line_df[["is_key_line"]] <- detect_codedoc_key_lines(line_df[["line"]])
  line_df[["is_key_line"]] <- line_df[["is_key_line"]] &
    line_df[["is_comment_line"]]

  key_line_df <- data.frame(
    key_line = line_df[["line"]][line_df[["is_key_line"]]],
    key_line_position = which(line_df[["is_key_line"]])
  )
  if (nrow(key_line_df) == 0L) {
    return(empty_output_df)
  }
  key_line_df[["key"]] <-  sub(
    paste0("[^@]*", codedoc_key_line_regex()),
    "",
    key_line_df[["key_line"]]
  )
  key_line_df[["key"]] <- gsub("(^\\s*)|(\\s*$)", "", key_line_df[["key"]])
  key_line_df <- key_line_df[
    order(key_line_df[["key"]], key_line_df[["key_line_position"]]),
  ]

  odd <- seq(min(nrow(key_line_df), 1L), (nrow(key_line_df) - 1L), 2L)
  block_df <- data.frame(
    key = key_line_df[["key"]][odd],
    first_block_line = key_line_df[["key_line_position"]][odd] + 1L,
    last_block_line = key_line_df[["key_line_position"]][odd + 1L] - 1L
  )
  key_df <- data.frame(key = unique(block_df[["key"]]))
  key_df[["key_count"]] <- as.integer(table(key_line_df[["key"]]))
  key_df[["is_misspecified_key"]] <- key_df[["key_count"]] %% 2L != 0L


  if (nrow(block_df) > 0L) {
    if (any(key_df[["is_misspecified_key"]])) {
      misspecified_key_set <- key_df[["key"]][key_df[["is_misspecified_key"]]]
      stop("Following keys did not appear an even number of times in file ",
           deparse(text_file_paths), ": ",
           deparse(misspecified_key_set))
    }
    block_df[["comment_block"]] <- lapply(1:nrow(block_df), function(key_no) {
      first_line_pos <- block_df[["first_block_line"]][key_no]
      last_line_pos <- block_df[["last_block_line"]][key_no]
      block_line_positions <- first_line_pos:last_line_pos
      lines <- line_df[["line"]][block_line_positions]
      lines <- clean_comment_lines(lines)
      line_is_key_line <- line_df[["is_key_line"]][block_line_positions]
      lines <- lines[!line_is_key_line]
      lines[]
    })
  }

  output_block_df_col_nms <- c(
    "key", "first_block_line", "last_block_line",
    "comment_block"
  )
  out <- cbind(text_file_path = rep(text_file_paths, nrow(block_df)),
               block_df[, output_block_df_col_nms])
  return(out)
}





