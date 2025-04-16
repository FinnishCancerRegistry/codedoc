git_is_installed__ <- function() {
  tryCatch(
    expr = is.character(
      system2("git", "version", stdout = TRUE, stderr = TRUE)
    ),
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}

git_used_here__ <- function() {
  tryCatch(
    expr = is.character(
      system2("git", "status", stdout = TRUE, stderr = TRUE)
    ),
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}

git_ignores__ <- function(file_paths) {
  dbc::assert_prod_input_file_exists(file_paths)
  not_ignored_file_paths <- system2(
    "git",
    c("ls-files", "--cached", "--others", "--exclude-standard"),
    stdout = TRUE
  )
  not_ignored_file_paths <- normalizePath(
    not_ignored_file_paths,
    winslash = "/",
    mustWork = FALSE
  )
  file_paths <- normalizePath(
    file_paths,
    winslash = "/",
    mustWork = FALSE
  )
  !file_paths %in% not_ignored_file_paths
}
