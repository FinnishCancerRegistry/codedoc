




#' @title Render Code Documentation
#' @description
#' Render using [rmarkdown::render] a documentation file using extracted
#' code comment blocks.
#' @param key_df `[data.frame]` (mandatory, no default)
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
#' determined internally; the default is
#' ```
#' list(output_file = "codedoc.md",
#'      output_dir = getwd(),
#'      quiet = TRUE)
#' ```
#'
#' @section Template:
#'
#' TODO
#' @export
render_codedoc <- function(
  key_df,
  template_file_path = NULL,
  writeLines_arg_list = list(),
  render_arg_list = list()
) {
  dbc::assert_is_data.frame_with_required_names(
    key_df,
    required_names = c("key", "comment_block", "text_file_path")
  )
  dbc::assert_is_list(render_arg_list)
  dbc::assert_vector_elems_are_in_set(
    x = names(render_arg_list),
    set = names(formals(rmarkdown::render))
  )
  dbc::assert_vector_elems_are_in_set(
    x = names(writeLines_arg_list),
    set = names(formals(writeLines))
  )
  # dbc::assert_is_one_of(
  #   x = template_file_path,
  #   funs = c("assert_is_NULL", "assert_file_exists")
  # )

  if (is.null(template_file_path)) {
    template_lines <- unlist(lapply(1:nrow(key_df), function(key_no) {
      c(
        paste0("## ", key_df[["key"]][key_no]),
        "",
        key_df[["comment_block"]][key_no],
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
      paste0("- ", key_df[["text_file_path"]]),
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
  writeLines_arg_list[["text"]] <- template_lines
  writeLines_arg_list[["con"]] <- tmp_rmd_file_path
  do.call(writeLines, writeLines_arg_list, quote = TRUE)

  default_render_arg_list <- list(output_file = "codedoc.md",
                                  output_dir = getwd(),
                                  quiet = TRUE)
  use_render_arg_list <- default_render_arg_list
  use_render_arg_list[names(render_arg_list)] <- render_arg_list
  use_render_arg_list[["input"]] <- tmp_rmd_file_path
  if (!is.environment(use_render_arg_list[["envir"]])) {
    use_render_arg_list[["envir"]] <- new.env(parent = parent.frame(1L))
  }
  use_render_arg_list[["envir"]][["key_df"]] <- key_df
  use_render_arg_list[["envir"]][["comment_block_by_key"]] <- structure(
    key_df[["comment_block"]],
    names = key_df[["key"]]
  )
  use_render_arg_list[["envir"]][["codedoc_text"]] <- function(key) {
    dbc::assert_is_character_nonNA_atom(key)
    dbc::assert_atom_is_in_set(key, set = key_df[["key"]])
    paste0(use_render_arg_list[["envir"]][["comment_block_by_key"]][[key]],
           collapse = "\n")
  }
  do.call(rmarkdown::render, use_render_arg_list)
}









