




#' @title Example Templates
#' @description
#' Retrieve example codedoc templates.
#' @param example_template_file_name `[character]` (mandatory, no default)
#'
#' name of example template file
#' @name example_template
NULL

#' @rdname example_template
#' @details
#' - `example_template_dir_path` gets the path to the dir containing example
#'    templates in this package
#' @export
example_template_dir_path <- function() {
  system.file("example_templates", package = "codedoc")
}

#' @rdname example_template
#' @details
#' - `example_template_file_names` gets the names example template files
#' @export
example_template_file_names <- function() {
  dir(example_template_dir_path())
}

#' @rdname example_template
#' @details
#' - `example_template_file_paths` gets the full paths to example template files
#' @export
example_template_file_paths <- function() {
  dir(example_template_dir_path(), full.names = TRUE)
}
#' @rdname example_template
#' @details
#' - `example_template_file_path` gets the path to an example template based on
#'   its name
#' @export
example_template_file_path <- function(
  example_template_file_name
) {
  dbc::assert_is_character_nonNA_atom(example_template_file_name)
  example_template_file_names <- example_template_file_names()
  dbc::assert_atom_is_in_set(
    x = example_template_file_name,
    set = example_template_file_names
  )

  example_template_file_path <- paste0(
    example_template_dir_path(), "/", example_template_file_name
  )
  return(example_template_file_path)
}

#' @rdname example_template
#' @details
#' - `example_template_lines` reads the lines of an example template based on
#'   its name
#' @export
example_template_lines <- function(
  example_template_file_name
) {
  readLines(example_template_file_path(example_template_file_name))
}




#' @title Example Text Files
#' @description
#' Retrieve example text files processable by codedoc.
#' @param example_text_file_name `[character]` (mandatory, no default)
#'
#' name of example text file
#' @name example_text_file
NULL

#' @rdname example_text_file
#' @details
#' - `example_text_file_dir` gets the path to the dir containing example text
#'   files
#' @export
example_text_file_dir <- function() {
  system.file("example_text_files", package = "codedoc")
}

#' @rdname example_text_file
#' @details
#' - `example_text_file_names` gets the names of all example text files in
#'   this package
#' @export
example_text_file_names <- function() {
  dir(example_text_file_dir())
}

#' @rdname example_text_file
#' @details
#' - `example_text_file_paths` gets the full paths to all example text files in
#'   this package
#' @export
example_text_file_paths <- function() {
  dir(example_text_file_dir(), full.names = TRUE)
}
#' @rdname example_text_file
#' @details
#' - `example_text_file_path` gets the full path to an example text file
#'   based on its name
#' @export
example_text_file_path <- function(example_text_file_name) {
  dbc::assert_is_character_nonNA_atom(example_text_file_name)
  dbc::assert_atom_is_in_set(
    example_text_file_name,
    set = example_text_file_names()
  )
  paste0(example_text_file_dir(), "/", example_text_file_name)
}
#' @rdname example_text_file
#' @details
#' - `example_text_file_lines` reads the contents of an example text file
#'   into R based on its name
#' @export
example_text_file_lines <- function(example_text_file_name) {
  readLines(
    example_text_file_path(example_text_file_name)
  )
}



#' @title Render Code Documentation
#' @description
#' Render using [rmarkdown::render] a documentation file using extracted
#' code comment blocks.
#' @param block_df `[data.frame]` (mandatory, no default)
#'
#' a data.frame which must have columns `key`, `comment_block`, and
#' `text_file_path` as in the output of [extract_keyed_comment_blocks]
#' @param template_file_path `[NULL, character]` (optional, default `NULL`)
#'
#' - `NULL`: no template is used; see section Template
#' - `character`: path to a template file; see section Template
#' @param writeLines_arg_list `[list]` (optional, default `list()`)
#'
#' list of arguments passed to `writeLines` when writing temporary .rmd
#' file to render; `con` and `text` are always determined internally
#' @param render_arg_list `[list]` (optional, default `list()`)
#'
#' list of arguments passed to [rmarkdown::render]; `input` is always
#' determined internally; the default settings set internally in this function
#' are
#' ```
#' list(output_file = "codedoc.md",
#'      output_format = "md_document",
#'      output_dir = getwd(),
#'      quiet = TRUE,
#'      envir = new.env(parent = parent.frame(1L)))
#' ```
#'
#' where `envir` will by default be the environment where `render_codedoc` was
#' called. `envir` is additionally always populated by these objects:
#'
#' - `block_df`: the user-supplied arg
#' - `codedoc_lines`: a function with only the argument `key`, which must be
#'   one of the keys in `block_df`; the comment lines are returned for the
#'   corresponding key as a character vector
#' - `codedoc_text`: as `codedoc_lines`. but instead of the lines as a character
#'   vector (with one or more elements) this function returns exactly one
#'   string, formed by pasting the lines together with
#'   `paste0(lines, collapse = "\n")`; this is more convenient for rendering
#'   text in the template (see section Template)
#'
#'
#' @section Template:
#'
#' The template file must be an Rmarkdown file (see e.g. [rmarkdown::render]).
#' It's contents are completely up to you. You should make use the objects
#' created into the rendering environment (see `envir` in [rmarkdown::render])
#' by this function; see arg `render_arg_list`.
#'
#' Instead of using the `codedoc_` functions, one may simply write e.g.
#' `@codedoc_lines my_key` in the template file, and then the corresponding
#' comment block identified by key `my_key` will be inserted there before
#' rendering.
#'
#' See function [example_template_lines] for examples.
#' @details
#'
#' `render_codedoc` is intended to be used directly by the user
#' and not within other functions, whereas the converse holds for
#' `render_codedoc_`. See `help("dbc", package = "dbc")` for a discussion
#' on this distinction.
#' @name render_codedoc
NULL

report_codedoc_assertions <- function(
  block_df,
  template_file_path,
  writeLines_arg_list,
  render_arg_list,
  assertion_type
) {
  # report_df <- eval(quote(rbind(
  #   dbc::report_is_data.frame_with_required_names(
  #     block_df,
  #     required_names = c("key", "comment_block", "text_file_path")
  #   ),
  #   dbc::report_is_list(render_arg_list),
  #   dbc::report_vector_elems_are_in_set(
  #     x = names(render_arg_list),
  #     set = names(formals(rmarkdown::render))
  #   ),
  #   dbc::report_vector_elems_are_in_set(
  #     x = names(writeLines_arg_list),
  #     set = names(formals(writeLines))
  #   ),
  #   dbc::report_is_one_of(
  #     x = template_file_path,
  #     funs = c("report_is_NULL", "report_file_exists")
  #   )
  # )), envir = parent.frame(1L))
  #
  # dbc::report_to_assertion(
  #   report_df,
  #   assertion_type = assertion_type
  # )
}


#' @export
#' @rdname render_codedoc
render_codedoc <- function(
  block_df,
  template_file_path = NULL,
  writeLines_arg_list = list(),
  render_arg_list = list()
) {
  report_codedoc_assertions(
    block_df = block_df,
    template_file_path = template_file_path,
    writeLines_arg_list = writeLines_arg_list,
    render_arg_list = render_arg_list,
    assertion_type = "user_input"
  )

  call <- match.call()
  call[[1L]] <- quote(render_codedoc_)
  eval(call, envir = environment())
}

#' @export
#' @rdname render_codedoc
render_codedoc_ <- function(
  block_df,
  template_file_path = NULL,
  writeLines_arg_list = list(),
  render_arg_list = list()
) {
  requireNamespace("rmarkdown")
  report_codedoc_assertions(
    block_df = block_df,
    template_file_path = template_file_path,
    writeLines_arg_list = writeLines_arg_list,
    render_arg_list = render_arg_list,
    assertion_type = "prod_input"
  )

  key_set <- unique(block_df[["key"]])
  lines_by_key <- lapply(key_set, function(key) {
    key_block_list <- block_df[["comment_block"]][block_df[["key"]] == key]
    key_block_list <- lapply(key_block_list, c, "")
    lines <- unlist(key_block_list)
    lines
  })
  names(lines_by_key) <- key_set
  if (is.null(template_file_path)) {
    template_lines <- unlist(lapply(key_set, function(key) {
      c(
        paste0("## ", key),
        "",
        lines_by_key[[key]],
        ""
      )
    }))
    template_lines <- c(
      "---",
      "title: Code documentation",
      "---",
      "",
      "Based on the following files: ",
      "",
      paste0("- ", unique(block_df[["text_file_path"]])),
      "",
      template_lines
    )

  } else {
    template_lines <- readLines(template_file_path)
  }
  tmp_rmd_file_path <- tempfile(fileext = ".rmd")
  on.exit(
    if (file.exists(tmp_rmd_file_path)) {
      unlink(tmp_rmd_file_path)
    }
  )

  for (key in unique(block_df[["key"]])) {
    key_line_contents <- paste0("@codedoc_lines ", key)
    is_template_key_line <- template_lines == key_line_contents
    while (any(is_template_key_line)) {
      wh <- which(is_template_key_line)[1L]
      head_end <- (wh - 1L)
      tail_start <- (wh + 1L)
      template_lines <- c(
        template_lines[min(1L, head_end):head_end],
        lines_by_key[[key]],
        template_lines[tail_start:max(length(template_lines), tail_start)]
      )
      is_template_key_line <- template_lines == key_line_contents
    }

  }

  writeLines_arg_list[["text"]] <- template_lines
  writeLines_arg_list[["con"]] <- tmp_rmd_file_path
  do.call(writeLines, writeLines_arg_list, quote = TRUE)

  # @codedoc_comment_block default_render_arg_list
  default_render_arg_list <- list(output_file = "codedoc.md",
                                  output_format = "md_document",
                                  output_dir = getwd(),
                                  quiet = TRUE,
                                  envir = new.env(parent = parent.frame(1L)))
  # @codedoc_comment_block default_render_arg_list
  use_render_arg_list <- default_render_arg_list
  use_render_arg_list[names(render_arg_list)] <- render_arg_list
  use_render_arg_list[["input"]] <- tmp_rmd_file_path
  use_render_arg_list[["envir"]][["block_df"]] <- block_df
  use_render_arg_list[["envir"]][["codedoc_lines"]] <- function(key) {
    dbc::assert_is_character_nonNA_atom(key)
    dbc::assert_atom_is_in_set(key, set = names(lines_by_key))
    lines_by_key[[key]]
  }
  text_by_key <- lapply(lines_by_key, paste0, collapse = " ")
  use_render_arg_list[["envir"]][["codedoc_text"]] <- function(key) {
    dbc::assert_is_character_nonNA_atom(key)
    dbc::assert_atom_is_in_set(key, set = names(text_by_key))
    text_by_key[[key]]
  }
  do.call(rmarkdown::render, use_render_arg_list)
}









