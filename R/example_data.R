


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
