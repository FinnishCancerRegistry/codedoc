




codedoc_key_line_regex <- function() "(@codedoc_comment_block)"
detect_codedoc_key_lines <- function(x) {
  grepl(codedoc_key_line_regex(), x)
}





#' @title Read codedoc-formatted Code Comments
#' @description
#' Extracts blocks of specifically formatted comments from text file.
#' @param text_file_paths `[character]`
#' (mandatory, no default)
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
#' @param detect_allowed_keys `[function]`
#' (optional, default `function(x) rep(TRUE, length(x))`)
#'
#' a function which takes a character vector of keys as input and returns a
#' boolean vector of the same length, where an element is `TRUE` for keys
#' which should be retained (those filtered out should have `FALSE`);
#' the default keeps all keys.
#' @param readLines_arg_list `[list]`
#' (optional, default `list()`)
#'
#' list of arguments passed to [readLines]; `con` is always set to
#' an element of `text_file_paths`
#' @param string_interpolation_eval_env `[environment]`
#' (optional, default `parent.frame(1L)`)
#'
#' environment where string interpolation expressions are evaluated. by default
#' this is the environment where `extract_keyed_comment_blocks` or
#' `extract_keyed_comment_blocks_` is called. see Details for more information.
#' @details
#' `extract_keyed_comment_blocks` is intended to be used by the user and not
#' in other functions but is otherwise identical to
#' `extract_keyed_comment_blocks_`. `extract_keyed_comment_blocks_` is intended
#' to be used within other functions.
#' See `help(topic = "dbc", package = "dbc")` for discussion on this
#' distinction.
#'
#' Both insertion of comment blocks into other comment blocks and simple string
#' interpolation is possible. Insertion is performed before any interpolation.
#'
#' @eval codedoc_insert_comment_blocks_details()
#' @eval string_interpolation_details()
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
  detect_allowed_keys,
  readLines_arg_list,
  string_interpolation_eval_env,
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
        dbc::report_is_function_with_required_argument_names(
          detect_allowed_keys,
          required_argument_names = "x"
        ),
        dbc::report_is_environment(string_interpolation_eval_env),
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
  detect_allowed_keys = function(x) rep(TRUE, length(x)),
  readLines_arg_list = list(),
  string_interpolation_eval_env = parent.frame(1L)
) {
  extract_keyed_comment_blocks_assertions(
    text_file_paths,
    detect_comment_lines,
    clean_comment_lines,
    detect_allowed_keys,
    readLines_arg_list,
    string_interpolation_eval_env,
    assertion_type = "user_input"
  )
  extract_keyed_comment_blocks__(
    text_file_paths = text_file_paths,
    detect_comment_lines = detect_comment_lines,
    clean_comment_lines = clean_comment_lines,
    detect_allowed_keys = detect_allowed_keys,
    readLines_arg_list = readLines_arg_list,
    string_interpolation_eval_env = string_interpolation_eval_env,
    insert = TRUE,
    interpolate = TRUE
  )
}

#' @rdname extract_keyed_comment_blocks
#' @export
extract_keyed_comment_blocks_ <- function(
  text_file_paths,
  detect_comment_lines = function(x) grepl("^\\s*[#*]\\s*", x),
  clean_comment_lines = function(x) sub("^\\s*[#*]\\s*", "", x),
  detect_allowed_keys = function(x) rep(TRUE, length(x)),
  readLines_arg_list = list(),
  string_interpolation_eval_env = parent.frame(1L)
) {
  extract_keyed_comment_blocks_assertions(
    text_file_paths,
    detect_comment_lines,
    clean_comment_lines,
    detect_allowed_keys,
    readLines_arg_list,
    string_interpolation_eval_env,
    assertion_type = "prod_input"
  )
  extract_keyed_comment_blocks__(
    text_file_paths = text_file_paths,
    detect_comment_lines = detect_comment_lines,
    clean_comment_lines = clean_comment_lines,
    detect_allowed_keys = detect_allowed_keys,
    readLines_arg_list = readLines_arg_list,
    string_interpolation_eval_env = string_interpolation_eval_env,
    insert = TRUE,
    interpolate = TRUE
  )
}

extract_keyed_comment_blocks__ <- function(
  text_file_paths,
  detect_comment_lines = function(x) grepl("^\\s*[#*]\\s*", x),
  clean_comment_lines = function(x) sub("^\\s*[#*]\\s*", "", x),
  detect_allowed_keys = function(x) rep(TRUE, length(x)),
  readLines_arg_list = list(),
  string_interpolation_eval_env = parent.frame(1L),
  insert = TRUE,
  interpolate = TRUE
) {
  if (length(text_file_paths) > 1L) {
    call <- full_call()
    call[c("insert", "interpolate")] <- list(FALSE, FALSE)
    block_df_list <- lapply(text_file_paths, function(text_file_path) {
      call[["text_file_paths"]] <- text_file_path
      eval(call)
    })
    block_df <- do.call(rbind, block_df_list)
    if (insert) {
      block_df <- codedoc_insert_comment_blocks(block_df)
    }
    if (interpolate) {
      block_df[["comment_block"]] <- lapply(
        block_df[["comment_block"]],
        string_interpolation,
        env = string_interpolation_eval_env
      )
    }
    return(block_df)
  }

  readLines_arg_list[["con"]] <- text_file_paths

  line_df <- data.frame(
    line = do.call(readLines, readLines_arg_list),
    stringsAsFactors = FALSE
  )
  line_df[["is_comment_line"]] <- detect_comment_lines(line_df[["line"]])
  line_df[["is_key_line"]] <- detect_codedoc_key_lines(line_df[["line"]])
  line_df[["is_key_line"]] <- line_df[["is_key_line"]] &
    line_df[["is_comment_line"]]

  key_line_df <- data.frame(
    key_line = line_df[["line"]][line_df[["is_key_line"]]],
    key_line_position = which(line_df[["is_key_line"]]),
    stringsAsFactors = FALSE
  )
  if (nrow(key_line_df) == 0L) {
    empty_output_df <- data.frame(
      text_file_path = character(0),
      key = character(0),
      first_block_line = integer(0),
      last_block_line = integer(0),
      comment_block = vector("list", 0)
    )
    return(empty_output_df)
  }
  key_line_df[["key"]] <- sub(
    paste0("[^@]*", codedoc_key_line_regex()),
    "",
    key_line_df[["key_line"]]
  )
  key_line_df[["key"]] <- gsub("(^\\s*)|(\\s*$)", "", key_line_df[["key"]])

  # is_allowed_key <- detect_allowed_keys(key_line_df[["key"]])
  # key_line_df <- key_line_df[is_allowed_key, ]

  # sorting to ensure nested blocks don't mix up the order --- we want start
  # and stop lines to be after one another for each block.
  key_line_df <- key_line_df[
    order(key_line_df[["key"]], key_line_df[["key_line_position"]]),
  ]

  n_by_key <- table(key_line_df[["key"]])
  has_odd_number_of_keys <- n_by_key %% 2L != 0
  if (any(has_odd_number_of_keys)) {
    stop("Following keys did not appear an even number of times in file ",
         deparse(text_file_paths), ": ",
         deparse(names(n_by_key)[has_odd_number_of_keys]))
  }

  odd <- seq(min(nrow(key_line_df), 1L), (nrow(key_line_df) - 1L), 2L)
  block_df <- data.frame(
    text_file_path = rep(text_file_paths, length(odd)),
    key = key_line_df[["key"]][odd],
    first_block_line = key_line_df[["key_line_position"]][odd] + 1L,
    last_block_line = key_line_df[["key_line_position"]][odd + 1L] - 1L
  )
  key_df <- data.frame(key = unique(block_df[["key"]]))
  key_df[["key_count"]] <- as.integer(table(key_line_df[["key"]]))
  key_df[["is_misspecified_key"]] <- key_df[["key_count"]] %% 2L != 0L

  if (nrow(block_df) == 0L) {
    print(key_line_df)
    stop("Internal error: nrow(block_df) == 0; if you see this, complain to ",
         "the package maintainer; key_line_df printed above.")
  }
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
    lines
  })

  o <- order(block_df[["first_block_line"]], block_df[["last_block_line"]])
  block_df <- block_df[o, ]
  is_allowed_key <- detect_allowed_keys(block_df[["key"]])
  if (insert) {
    block_df <- codedoc_insert_comment_blocks(
      block_df,
      is_allowed_key
    )
  }
  block_df <- block_df[is_allowed_key, ]
  if (interpolate) {
    block_df[["comment_block"]] <- lapply(
      block_df[["comment_block"]],
      string_interpolation,
      env = string_interpolation_eval_env
    )
  }
  return(block_df)
}





codedoc_insert_comment_block_regex <- function() {
  "@codedoc_insert_comment_block"
}
codedoc_insert_comment_blocks <- function(
  block_df,
  subset
) {
  dbc::assert_prod_input_is_data.frame_with_required_names(
    block_df,
    required_names = c("comment_block", "key")
  )
  dbc::assert_prod_input_is_logical_nonNA_vector(subset)
  re <- codedoc_insert_comment_block_regex()

  block_df[["comment_block"]][subset] <- lapply(
    block_df[["comment_block"]][subset],
    function(lines) {
      # @codedoc_comment_block codedoc_insert_comment_blocks_details
      #
      # - lines with insert keys are detected using regex
      #   "${codedoc_insert_comment_block_regex()}"
      #
      # @codedoc_comment_block codedoc_insert_comment_blocks_details
      is_insert_line <- grepl(re, lines)
      tick <- 0L
      while (any(is_insert_line)) {
        # @codedoc_comment_block codedoc_insert_comment_blocks_details
        #
        # - all lines are passed through a maximum of ten times. this means
        #   that a recursion depth of ten is the maximum. recursion can occur
        #   if a comment block is inserted which in turn has one or more
        #   insert keys.
        #
        # @codedoc_comment_block codedoc_insert_comment_blocks_details
        tick <- tick + 1L
        if (tick == 10L) {
          stop("hit 10 passes in while loop when inserting comment blocks; ",
               "do you have self-referencing in a comment block?")
        }
        for (i in 1:sum(is_insert_line)) {
          # @codedoc_comment_block codedoc_insert_comment_blocks_details
          #
          # - insert keys are collected by removing the regex given above,
          #   anything preceding it, and all whitespaces after it
          #
          # @codedoc_comment_block codedoc_insert_comment_blocks_details
          insert_key_by_line <- sub(
            paste0(".*", re, "[ ]*"),
            "",
            lines
          )
          insert_key_by_line[!is_insert_line] <- NA_character_
          wh <- which(is_insert_line)[1L]
          insert_key <- insert_key_by_line[wh]
          if (!insert_key %in% block_df[["key"]]) {
            stop("found insert key which has no match in collected comment ",
                 "block keys. invalid key: ", deparse(insert_key),
                 "; collected keys: ", deparse(block_df[["key"]]))
          }
          # @codedoc_comment_block codedoc_insert_comment_blocks_details
          #
          # - each line with an insert key is effectively replaced with
          #   all lines in the comment block of that key (e.g. line with key
          #   "my_key" is replaced with all lines in comment block with key
          #   "my_key"). this is run separately for each detected insert key.
          #
          # @codedoc_comment_block codedoc_insert_comment_blocks_details
          add_lines <- unlist(
            block_df[["comment_block"]][block_df[["key"]] == insert_key]
          )
          head_lines <- character(0L)
          if (wh > 1L) {
            head_lines <- lines[1L:(wh - 1L)]
          }
          tail_lines <- character(0L)
          if (wh < length(lines)) {
            tail_lines <- lines[(wh + 1L):length(lines)]
          }
          lines <- c(head_lines, add_lines, tail_lines)
          is_insert_line <- grepl(re, lines)
        }
      }
      # @codedoc_comment_block codedoc_insert_comment_blocks_details
      #
      # - the result is still a character vector, but here the insert keys
      #   have been replaced with lines from the comment blocks under those keys
      #
      # @codedoc_comment_block codedoc_insert_comment_blocks_details
      return(lines)
    }
  )

  return(block_df)
}
codedoc_insert_comment_blocks_details <- function() {
  block_df <- codedoc::extract_keyed_comment_blocks_(
    text_file_paths = "R/collect_raw.R"
  )
  key <- "codedoc_insert_comment_blocks_details"
  block_df <- block_df[block_df[["key"]] == key, ]
  c(
    "@details",
    "Insertion of comment blocks into other comment blocks is implemented ",
    "as follows:",
    "",
    unlist(block_df[["comment_block"]])
  )
}






