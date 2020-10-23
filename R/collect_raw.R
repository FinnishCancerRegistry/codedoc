




codedoc_key_line_regex <- function() "(@doc)|(@codedoc)"
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
#' @export
extract_keyed_comment_blocks <- function(
  text_file_paths,
  detect_comment_lines = function(x) grepl("^\\s*[#*]\\s*", x),
  clean_comment_lines = function(x) sub("^\\s*[#*]\\s*", "", x),
  readLines_arg_list = list()
) {
  dbc::assert_is_character_nonNA_vector(text_file_paths)
  dbc::assert_file_exists(text_file_paths)
  dbc::assert_is_function_with_required_argument_names(
    detect_comment_lines,
    required_argument_names = "x"
  )
  dbc::assert_is_function_with_required_argument_names(
    clean_comment_lines,
    required_argument_names = "x"
  )
  dbc::assert_is_list(readLines_arg_list)

  if (length(text_file_paths) > 1L) {
    arg_list <- mget(names(formals(extract_keyed_comment_blocks)))
    key_df_list <- lapply(text_file_paths, function(text_file_path) {
      arg_list[["text_file_paths"]] <- text_file_path
      do.call(extract_keyed_comment_blocks, arg_list)
    })
    key_df <- do.call(rbind, key_df_list)
    return(key_df)
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
  key_line_df[["key"]] <-  sub(
    paste0("[^@]*", codedoc_key_line_regex()),
    "",
    key_line_df[["key_line"]]
  )
  key_line_df[["key"]] <- gsub("(^\\s*)|(\\s*$)", "", key_line_df[["key"]])

  key_df <- data.frame(
    key = unique(key_line_df[["key"]])
  )
  key_df[["key_count"]] <- as.integer(table(key_line_df[["key"]]))
  key_df[["is_misspecified_key"]] <- key_df[["key_count"]] != 2L

  output_key_df_col_nms <- c(
    "key", "first_block_line", "last_block_line", "comment_block"
  )
  key_df[["first_block_line"]] <- rep(NA_integer_, nrow(key_df))
  key_df[["last_block_line"]] <- rep(NA_integer_, nrow(key_df))
  key_df[["comment_block"]] <- rep(NA_character_, nrow(key_df))
  if (nrow(key_df) > 0L) {
    if (any(key_df[["is_misspecified_key"]])) {
      misspecified_key_set <- key_df[["key"]][key_df[["is_misspecified_key"]]]
      stop("Following keys did not appear exactly twice in file ",
           deparse(text_file_paths), ": ",
           deparse(misspecified_key_set))
    }

    key_df[["first_block_line"]] <- vapply(key_df[["key"]], function(key) {
      is_key <- key_line_df[["key"]] == key
      key_line_df[["key_line_position"]][is_key][1L] + 1L
    }, integer(1L))
    key_df[["last_block_line"]] <- vapply(key_df[["key"]], function(key) {
      is_key <- key_line_df[["key"]] == key
      key_line_df[["key_line_position"]][is_key][2L] - 1L
    }, integer(1L))

    key_df[["comment_block"]] <- lapply(1:nrow(key_df), function(key_no) {
      first_line_pos <- key_df[["first_block_line"]][key_no]
      last_line_pos <- key_df[["last_block_line"]][key_no]
      block_line_positions <- first_line_pos:last_line_pos
      clean_comment_lines(line_df[["line"]][block_line_positions])
    })
  }

  key_df <- cbind(text_file_path = rep(text_file_paths, nrow(key_df)),
                  key_df[, output_key_df_col_nms])
  return(key_df)
}





