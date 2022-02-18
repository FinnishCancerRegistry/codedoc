





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
#' @eval render_codedoc_arg_render_arg_list()
#' @template arg_assertion_type
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

render_codedoc_assertions <- function(
  block_df,
  template_file_path,
  writeLines_arg_list,
  render_arg_list,
  call = NULL,
  assertion_type = "input"
) {
  call <- dbc::handle_arg_call(call)
  dbc::assert_is_data.frame_with_required_names(
    block_df,
    required_names = c("key", "comment_block", "text_file_path"),
    assertion_type = assertion_type,
    call = call
  )
  dbc::assert_is_list(render_arg_list,
                      assertion_type = assertion_type,
                      call = call)
  dbc::assert_vector_elems_are_in_set(
    x = names(render_arg_list),
    set = names(formals(rmarkdown::render)),
    assertion_type = assertion_type,
    call = call
  )
  dbc::assert_vector_elems_are_in_set(
    x = names(writeLines_arg_list),
    set = names(formals(writeLines)),
    assertion_type = assertion_type,
    call = call
  )
  dbc::assert_is_one_of(
    x = template_file_path,
    funs = c("report_is_NULL", "report_file_exists"),
    assertion_type = assertion_type,
    call = call
  )
}


#' @export
#' @rdname render_codedoc
render_codedoc <- function(
  block_df,
  template_file_path = NULL,
  writeLines_arg_list = list(),
  render_arg_list = list(),
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::render_codedoc", "2022-02-18", "0.3.0")
  # `[codedoc:render_codedoc]` gained arg `assertion_type`.
  # @codedoc_comment_block news("codedoc::render_codedoc", "2022-02-18", "0.3.0")
  render_codedoc__(
    block_df = block_df,
    template_file_path = template_file_path,
    writeLines_arg_list = writeLines_arg_list,
    render_arg_list = render_arg_list,
    assertion_type = assertion_type,
    call = match.call()
  )
}

#' @export
#' @rdname render_codedoc
render_codedoc_ <- function(
  block_df,
  template_file_path = NULL,
  writeLines_arg_list = list(),
  render_arg_list = list()
) {
  # @codedoc_comment_block news("codedoc::render_codedoc_", "2022-02-18", "0.3.0")
  # `[codedoc:render_codedoc_]` marked for deprecation.
  # Use `[codedoc:render_codedoc]`.
  # @codedoc_comment_block news("codedoc::render_codedoc_", "2022-02-18", "0.3.0")
  warning("codedoc::render_codedoc_ is deprecated and will be ",
          "removed in codedoc version 0.4.0, planned for 2022-04-01. ",
          "Use codedoc::render_codedoc.")
  render_codedoc__(
    block_df = block_df,
    template_file_path = template_file_path,
    writeLines_arg_list = writeLines_arg_list,
    render_arg_list = render_arg_list,
    assertion_type = "prod_input",
    call = match.call()
  )
}

render_codedoc__ <- function(
  block_df,
  template_file_path = NULL,
  writeLines_arg_list = list(),
  render_arg_list = list(),
  assertion_type = "input",
  call = NULL
) {
  requireNamespace("rmarkdown")

  render_codedoc_assertions(
    block_df = block_df,
    template_file_path = template_file_path,
    writeLines_arg_list = writeLines_arg_list,
    render_arg_list = render_arg_list,
    assertion_type = assertion_type,
    call = call
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
  tmp_rmd_file_path <- tempfile(pattern = "render_codedoc__", fileext = ".rmd")
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
      head_stop <- wh - 1L
      head_start <- ifelse(head_stop == 0L, 0L, 1L)
      head <- head_start:head_stop
      tail_start <- wh + 1L
      tail_start <- ifelse(wh == length(template_lines), 0L, tail_start)
      tail_stop <- ifelse(tail_start == 0L, 0L, length(template_lines))
      tail <- tail_start:tail_stop

      template_lines <- c(
        template_lines[head],
        lines_by_key[[key]],
        template_lines[tail]
      )
      is_template_key_line <- template_lines == key_line_contents
    }

  }

  writeLines_arg_list[["text"]] <- template_lines
  writeLines_arg_list[["con"]] <- tmp_rmd_file_path
  do.call(writeLines, writeLines_arg_list, quote = TRUE)
  # @codedoc_comment_block default_render_arg_list_1
  default_render_arg_list <- list(
    output_file = "codedoc.md",
    output_format = "md_document",
    output_dir = getwd(),
    quiet = TRUE,
    envir = new.env(parent = parent.frame(1L)),
    knit_root_dir = if (is.null(template_file_path)) getwd() else
      normalizePath(dirname(template_file_path))
  )
  # @codedoc_comment_block default_render_arg_list_1
  use_render_arg_list <- default_render_arg_list
  use_render_arg_list[names(render_arg_list)] <- render_arg_list
  use_render_arg_list[["input"]] <- tmp_rmd_file_path
  # @codedoc_comment_block default_render_arg_list_2
  # `envir` is always populated by these objects:
  #
  # - `block_df`: the user-supplied arg
  # - `codedoc_lines`: a function with only the argument `key`, which must be
  #   one of the keys in `block_df`; the comment lines are returned for the
  #   corresponding key as a character vector
  # - `codedoc_text`: as `codedoc_lines`. but instead of the lines as a
  #   character vector (with one or more elements) this function returns exactly
  #   one string, formed by pasting the lines together with
  #   `paste0(lines, collapse = "\n")`; this is more convenient for rendering
  #   text in the template (see section Template)
  # @codedoc_comment_block default_render_arg_list_2
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

render_codedoc_arg_render_arg_list <- function() {
  block_df <- codedoc::extract_keyed_comment_blocks(
    text_file_paths = "R/render.R",
    detect_allowed_keys = function(x) grepl("default_render_arg_list", x)
  )
  wh_1 <- which(block_df[["key"]] == "default_render_arg_list_1")
  wh_2 <- which(block_df[["key"]] == "default_render_arg_list_2")
  lines <- c(
    "@param render_arg_list `[list]` (optional, default `list()`)",
    "list of arguments passed to [rmarkdown::render]; `input` is always",
    "determined internally; the default settings set internally in this function",
    "are",
    "```",
    block_df[["comment_block"]][[wh_1]],
    "```",
    "",
    "where `envir` will by default be the environment where `render_codedoc` was",
    "called.",
    "",
    block_df[["comment_block"]][[wh_2]]
  )
  return(lines)
}







