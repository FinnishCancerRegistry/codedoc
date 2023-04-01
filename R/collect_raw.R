




codedoc_key_line_regex <- function() "(@codedoc_comment_block)"
detect_codedoc_key_lines <- function(x) {
  grepl(codedoc_key_line_regex(), x)
}





#' @title Read codedoc-formatted Code Comments
#' @description
#' Extracts blocks of specifically formatted comments from text file.
#' @param readLines_arg_list `[list]`
#' (optional, default `list(warn = FALSE)`)
#'
#' list of arguments passed to [readLines]; `con` is always set to
#' an element of `text_file_paths`
#' @param string_interpolation_eval_env `[environment]`
#' (optional, default `parent.frame(1L)`)
#'
#' Environment where string interpolation expressions are evaluated. By default
#' this is the environment where `extract_keyed_comment_blocks` is called.
#' See Details for more information.
#' @eval c(
#'   codedoc::codedoc_lines(
#'     detect_allowed_keys = "codedoc:::extract_keyed_comment_blocks__"
#'   ),
#'   codedoc::codedoc_lines(
#'     detect_allowed_keys = "examples(codedoc::extract_keyed_comment_blocks)"
#'   )
#' )
#' @name extract_keyed_comment_blocks
NULL

assert_arg_text_file_paths <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = "input"
) {
  dbc::assert_is_one_of(
    x,
    x_nm = dbc::handle_arg_x_nm(x_nm),
    funs = list(
      dbc::report_is_NULL,
      dbc::report_file_exists
    ),
    call = dbc::handle_arg_call(call),
    assertion_type = assertion_type
  )
}

extract_keyed_comment_blocks_assertions__ <- function(
  text_file_paths,
  detect_comment_lines,
  clean_comment_lines,
  detect_allowed_keys,
  sort_by,
  readLines_arg_list,
  string_interpolation_eval_env,
  assertion_type,
  call = NULL
) {
  call <- dbc::handle_arg_call(call)

  assert_arg_text_file_paths(text_file_paths)

  dbc::assert_is_one_of(
    detect_comment_lines,
    funs = list(
      dbc::report_is_function,
      dbc::report_is_character_nonNA_atom
    ),
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_is_one_of(
    clean_comment_lines,
    funs = list(
      dbc::report_is_function,
      dbc::report_is_character_nonNA_atom
    ),
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_is_one_of(
    detect_allowed_keys,
    funs = list(
      dbc::report_is_function,
      dbc::report_is_character_nonNA_atom
    ),
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_is_one_of(
    sort_by,
    funs = list(
      dbc::report_is_NULL,
      dbc::report_is_character_nonNA_vector
    ),
    call = call,
    assertion_type = assertion_type
  )
  if (is.character(sort_by)) {
    dbc::assert_vector_elems_are_in_set(
      sort_by,
      set = names(empty_comment_block_df())
    )
  }

  dbc::assert_is_environment(string_interpolation_eval_env, call = call,
                             assertion_type = assertion_type)
  dbc::assert_is_list(readLines_arg_list, call = call,
                      assertion_type = assertion_type)
}

#' @template arg_assertion_type
#' @rdname extract_keyed_comment_blocks
#' @export
extract_keyed_comment_blocks <- function(
  text_file_paths      = NULL,
  detect_comment_lines = "^\\s*[#*]\\s?",
  clean_comment_lines  = "^\\s*[#*]\\s?",
  detect_allowed_keys  = "",
  sort_by = NULL,
  readLines_arg_list   = list(warn = FALSE),
  string_interpolation_eval_env = parent.frame(1L),
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2022-02-17", "0.2.15")
  # Changes to function `[codedoc::extract_keyed_comment_blocks]` arguments:
  #
  # - `text_file_paths`: allowed to also be `NULL`; default now `NULL`
  # - `detect_comment_lines`: allowed to also be `character`; default now
  #   `"^\\s*[#*]\\s*"`
  # - `clean_comment_lines`: allowed to also be `character`; default now
  #   `"^\\s*[#*]\\s*"`
  # - `detect_allowed_keys`: allowed to also be `character`; default now
  #   `""`
  #
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2022-02-17", "0.2.15")

  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2022-02-18", "0.3.0")
  # `[codedoc:extract_keyed_comment_blocks]` gained arg `assertion_type`.
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2022-02-18", "0.3.0")

  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-04-01", "0.3.8")
  # `[codedoc:extract_keyed_comment_blocks]` default args changed:
  # `detect_comment_lines = "^\\s*[#*]\\s*"`
  # -> `detect_comment_lines = "^\\s*[#*]\\s?"`
  # `clean_comment_lines  = "^\\s*[#*]\\s?"`
  # -> `clean_comment_lines  = "^\\s*[#*]\\s*"`
  #
  # This will improves retaining of indentation in comment blocks.
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-04-01", "0.3.8")

  # @codedoc_comment_block examples(codedoc::extract_keyed_comment_blocks)
  # @examples
  #
  # # codedoc::extract_keyed_comment_blocks
  # @codedoc_comment_block R_package_example(codedoc)
  # block_df <- codedoc::extract_keyed_comment_blocks(
  #   text_file_paths = codedoc::example_text_file_path("r_script.R")
  # )
  # print(block_df)
  # @codedoc_comment_block R_package_example(codedoc)
  # @codedoc_comment_block examples(codedoc::extract_keyed_comment_blocks)

  extract_keyed_comment_blocks__(
    text_file_paths = text_file_paths,
    detect_comment_lines = detect_comment_lines,
    clean_comment_lines = clean_comment_lines,
    detect_allowed_keys = detect_allowed_keys,
    readLines_arg_list = readLines_arg_list,
    sort_by = sort_by,
    string_interpolation_eval_env = string_interpolation_eval_env,
    insert = TRUE,
    interpolate = TRUE,
    assertion_type = assertion_type,
    call = match.call()
  )
}

default_text_file_paths <- function() {
  # @codedoc_comment_block codedoc:::default_text_file_paths
  # ```r
  # dir(
  #   path = getwd(),
  #   pattern = "[.]((r)|(rmd))$",
  #   full.names = TRUE,
  #   recursive = TRUE,
  #   ignore.case = TRUE
  # )
  # ```
  # @codedoc_comment_block codedoc:::default_text_file_paths

  dir(
    path = getwd(),
    pattern = "[.]((r)|(rmd))$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
}

empty_comment_block_df <- function() {
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2022-07-05", "0.3.2")
  # `codedoc::extract_keyed_comment_block` ensured to return a proper empty
  # data.frame when nothing was extracted. Column `comment_block` was
  # previously missing.
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2022-07-05", "0.3.2")
  df <- data.frame(
    text_file_path = character(0),
    key = character(0),
    first_block_line = integer(0),
    last_block_line = integer(0)
  )
  df[["comment_block"]] <- list()
  return(df)
}

extract_keyed_comment_blocks__ <- function(
  text_file_paths = NULL,
  detect_comment_lines = "^\\s*[#*]\\s*",
  clean_comment_lines = "^\\s*[#*]\\s*",
  detect_allowed_keys = "",
  sort_by = NULL,
  readLines_arg_list = list(warn = FALSE),
  string_interpolation_eval_env = parent.frame(1L),
  insert = TRUE,
  interpolate = TRUE,
  assertion_type = "input",
  call = NULL
) {
  main_call <- dbc::handle_arg_call(call)
  extract_keyed_comment_blocks_assertions__(
    text_file_paths,
    detect_comment_lines,
    clean_comment_lines,
    detect_allowed_keys,
    sort_by,
    readLines_arg_list,
    string_interpolation_eval_env,
    assertion_type = assertion_type,
    call = main_call
  )

  if (is.null(text_file_paths)) {
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    # @param text_file_paths `[NULL, character]` (default `NULL`)
    #
    # - `NULL`: collect text file paths using call
    # @codedoc_insert_comment_block codedoc:::default_text_file_paths
    #
    # - `character`: use these text files
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__

    # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-03-14", "0.3.6")
    # `[codedoc:extract_keyed_comment_blocks]` arg `text_file_paths` default
    # improved --- now uses `pattern = "[.]((r)|(rmd))$"` in place of
    # `pattern = "((r)|(rmd))$"`.
    # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-03-14", "0.3.6")

    text_file_paths <- default_text_file_paths()
  }

  if (is.character(detect_comment_lines)) {
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    # @param detect_comment_lines `[character, function]`
    # (default `"^\\s*[#*]"`)
    #
    # - `character`: pass this string as arg `pattern` to `[grepl]`
    # - `function`: a function which takes lines of text as input and outputs a
    #   logical vector of the same length as input which is `TRUE` when the line
    #   is a comment line
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    detect_comment_lines_regex <- detect_comment_lines
    detect_comment_lines <- function(x) {
      grepl(detect_comment_lines_regex, x)
    }
  }
  if (is.character(clean_comment_lines)) {
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    # @param clean_comment_lines `[character, function]`
    # (default `"^\\s*[#*]\\s*"`)
    #
    # - `character`: pass this string as arg `pattern` to `[sub]` call
    #   `sub(pattern, "", x, perl = TRUE)` where `x` are the lines from a
    #   text file
    # - `function`: a function which takes lines of text as input and outputs a
    #   character vector of the same length as input; this function takes comment
    #   lines and should strip any comment characters preceding the text itself
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    clean_comment_lines_regex <- clean_comment_lines
    clean_comment_lines <- function(x) {
      sub(clean_comment_lines_regex, "", x, perl = TRUE)
    }
  }

  if (is.null(sort_by)) {
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    # @param sort_by `[NULL, character]` (default `NULL`)
    #
    # Names of columns by which to sort output `block_df`.
    # Each column name must be one of the following:
    # ${deparse(names(codedoc:::empty_comment_block_df()))}
    #
    # - `NULL`: Use `c("text_file_path", "first_block_line")`.
    # - `character`: Sort output `block_df` by these columns.
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    sort_by <- c("text_file_path", "first_block_line")
  }

  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  # @details
  #
  # **Text extraction**
  #
  # The following steps are performed to extract text data from each given
  # `text_file_paths` path:
  #
  # @codedoc_insert_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  #
  # All results from all given `text_file_paths` are collected into one
  # `data.frame`.
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  block_df <- lapply(text_file_paths, function(text_file_path) {
    extract_keyed_comment_blocks_from_one_file(
      text_file_path = text_file_path,
      detect_comment_lines = detect_comment_lines,
      clean_comment_lines = clean_comment_lines,
      readLines_arg_list = readLines_arg_list
    )
  })
  block_df <- do.call(rbind, block_df)
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  # @details
  #
  # **Comment block insertion**
  #
  # Comment block insertion is performed as follows:
  #
  # - Allowed keys are identified using `detect_allowed_keys`
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__

  if (is.character(detect_allowed_keys)) {
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    # @param detect_allowed_keys `[character, function]`
    # (optional, default `""`)
    #
    # - `character`: pass this string as arg `pattern` to `[grepl]`
    # - `function`:   a function which takes a character vector of keys as input
    #    and returns a boolean vector of the same length, where an element is
    #    `TRUE` for keys which should be retained (those filtered out should have
    #    `FALSE`)
    #
    # this filtering does not affect which comment blocks are read into R; instead
    # it affects which are processed (inserting + interpolation) and which keys are
    # returned in output. therefore, comment blocks that need to be inserted into
    # the ones you want to retain in output are kept for the insertion phase.
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    detect_allowed_keys_regex <- detect_allowed_keys
    detect_allowed_keys <- function(x) {
      grepl(detect_allowed_keys_regex, x)
    }
  }
  is_allowed_key <- detect_allowed_keys(block_df[["key"]])
  if (insert) {
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    # @details
    #
    # - Insertion is performed on only the blocks with allowed keys
    #   (but blocks with non-allowed keys can contain data for insertion into
    #   blocks with allowed keys)
    #
    # @codedoc_insert_comment_block codedoc:::codedoc_insert_comment_blocks
    #
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    block_df <- codedoc_insert_comment_blocks(
      block_df = block_df,
      subset = is_allowed_key
    )
  }
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  # @details
  #
  # After insertion, all data with non-allowed keys are dropped.
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  block_df <- block_df[is_allowed_key, ]
  if (interpolate) {
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    # @details
    #
    # **String interpolation**
    #
    # String interpolation is applied after any comment block insertions.
    # It is performed on both each `comment_block` element and on each
    # `key`. This means that even the keys can be defined programmatically.
    # Interpolation is performed as follows:
    #
    # @codedoc_insert_comment_block codedoc:::string_interpolation
    #
    # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
    block_df[["comment_block"]] <- lapply(
      seq_along(block_df[["comment_block"]]),
      function(i) {
        string_interpolation(
          x = block_df[["comment_block"]][[i]],
          env = string_interpolation_eval_env,
          debug_data = list(
            text_file_path = block_df[["text_file_path"]][i],
            first_line_no = block_df[["first_block_line"]][i] - 1L
          )
        )
      }
    )
    block_df[["key"]] <- vapply(
      seq_along(block_df[["key"]]),
      function(i) {
        string_interpolation(
          block_df[["key"]][i],
          env = string_interpolation_eval_env,
          debug_data = list(
            text_file_path = block_df[["text_file_path"]][i],
            first_line_no = block_df[["first_block_line"]][i] - 1L
          )
        )
      },
      character(1L)
    )
  }
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  # @details
  #
  # **Last steps**
  #
  # Finally, duplicate rows in the `data.frame` of comment blocks are removed,
  # where duplicates those with non-unique combinations of the `key` and
  # `comment_block` columns. This means that the same data collected from
  # two different files (or the same file) are not retained.
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  block_df <- block_df[!duplicated(block_df[, c("key", "comment_block")]), ]

  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  # The `data.frame` is sorted into descending order by columns
  # given via arg `sort_by`.
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2022-10-21", "0.3.3")
  # The `data.frame` of collected blocks is now always sorted by
  # `text_file_path` and `first_block_line`. The order of results was this
  # also before, but now it has been made explicit in code and documentation.
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2022-10-21", "0.3.3")
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-02-01", "0.3.4")
  # User can now easily sort output `block_df` by columns of their choice via
  # new arg `sort_by`.
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-02-01", "0.3.4")
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-02-01", "0.3.5")
  # `sort_by` arg now actually works.
  # @codedoc_comment_block news("codedoc::extract_keyed_comment_blocks", "2023-02-01", "0.3.5")
  
  bdf_order_cols <- as.list(block_df[, sort_by, drop = FALSE])
  bdf_order <- do.call(order, bdf_order_cols, quote = TRUE)
  block_df <- block_df[bdf_order, ]

  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  # This concludes comment block extraction.
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks__
  return(block_df)
}

extract_keyed_comment_blocks_from_one_file <- function(
  text_file_path,
  detect_comment_lines = function(x) grepl("^\\s*[#*]\\s*", x),
  clean_comment_lines = function(x) sub("^\\s*[#*]\\s*", "", x),
  readLines_arg_list = list(warn = FALSE)
) {
  readLines_arg_list[["con"]] <- text_file_path

  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  #
  # - Each whole text file is read into R using [readLines]
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  line_df <- data.frame(
    line = do.call(readLines, readLines_arg_list),
    stringsAsFactors = FALSE
  )
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  #
  # - Comment lines are identified by calling `detect_comment_lines`
  # - Key lines are identified by calling `detect_codedoc_key_lines`
  #   on identified comment lines
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  line_df[["is_comment_line"]] <- detect_comment_lines(line_df[["line"]])
  line_df[["is_key_line"]] <- line_df[["is_comment_line"]]
  line_df[["is_key_line"]][line_df[["is_comment_line"]]] <-
    detect_codedoc_key_lines(line_df[["line"]][line_df[["is_comment_line"]]])

  key_line_df <- data.frame(
    key_line = line_df[["line"]][line_df[["is_key_line"]]],
    key_line_position = which(line_df[["is_key_line"]]),
    stringsAsFactors = FALSE
  )
  if (nrow(key_line_df) == 0L) {
    return(empty_comment_block_df())
  }
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  #
  # - Keys are collected
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  key_line_df[["key"]] <- sub(
    paste0("[^@]*", codedoc_key_line_regex()),
    "",
    key_line_df[["key_line"]]
  )
  key_line_df[["key"]] <- gsub("(^\\s*)|(\\s*$)", "", key_line_df[["key"]])

  # sorting to ensure nested blocks don't mix up the order --- we want start
  # and stop lines to be after one another for each block.
  key_line_df <- key_line_df[
    order(key_line_df[["key"]], key_line_df[["key_line_position"]]),
  ]

  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  #
  # - The number of appearances of each key is verified to be an even number
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  n_by_key <- table(key_line_df[["key"]])
  has_odd_number_of_keys <- n_by_key %% 2L != 0
  if (any(has_odd_number_of_keys)) {
    stop("Following keys did not appear an even number of times in file ",
         deparse(text_file_path), ": ",
         deparse(names(n_by_key)[has_odd_number_of_keys]))
  }

  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  #
  # - Each comment block is collected from the text
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  odd <- seq(min(nrow(key_line_df), 1L), (nrow(key_line_df) - 1L), 2L)
  block_df <- data.frame(
    text_file_path = rep(text_file_path, length(odd)),
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
         deparse(text_file_path), ": ",
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

  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  #
  # - Comment blocks are sorted by the first and last line number of the block,
  #   respectively
  #
  # @codedoc_comment_block codedoc:::extract_keyed_comment_blocks_from_one_file
  o <- order(block_df[["first_block_line"]], block_df[["last_block_line"]])
  block_df <- block_df[o, ]
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
  dbc::assert_prod_input_is_of_length(subset, expected_length = nrow(block_df))
  re <- codedoc_insert_comment_block_regex()
  block_df[["comment_block"]][subset] <- lapply(
    block_df[["comment_block"]][subset],
    function(lines) {
      # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
      #
      # - lines with insert keys are detected using regex
      #   "${codedoc:::codedoc_insert_comment_block_regex()}"
      #
      # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
      is_insert_line <- grepl(re, lines)
      tick <- 0L
      while (any(is_insert_line)) {
        # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
        #
        # - all lines are passed through a maximum of ten times. this means
        #   that a recursion depth of ten is the maximum. recursion can occur
        #   if a comment block is inserted which in turn has one or more
        #   insert keys.
        #
        # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
        tick <- tick + 1L
        if (tick == 10L) {
          stop("hit 10 passes in while loop when inserting comment blocks; ",
               "do you have self-referencing in a comment block?")
        }
        for (i in 1:sum(is_insert_line)) {
          # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
          #
          # - insert keys are collected by removing the regex given above,
          #   anything preceding it, and all whitespaces after it
          #
          # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
          insert_key_by_line <- rep(NA_character_, length(lines))
          insert_key_by_line[is_insert_line] <- sub(
            paste0("^.*(", re, ")\\s*"),
            "",
            lines[is_insert_line]
          )
          wh <- which(is_insert_line)[1L]
          insert_key <- insert_key_by_line[wh]
          if (!insert_key %in% block_df[["key"]]) {
            stop("found insert key which has no match in collected comment ",
                 "block keys. invalid key: ", deparse(insert_key),
                 "; collected keys: ", deparse(block_df[["key"]]))
          }
          # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
          #
          # - each line with an insert key is effectively replaced with
          #   all lines in the comment block of that key (e.g. line with key
          #   "my_key" is replaced with all lines in comment block with key
          #   "my_key"). this is run separately for each detected insert key.
          #
          # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
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
      # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
      #
      # - the result is still a character vector, but here the insert keys
      #   have been replaced with lines from the comment blocks under those keys
      #
      # @codedoc_comment_block codedoc:::codedoc_insert_comment_blocks
      return(lines)
    }
  )

  return(block_df)
}






