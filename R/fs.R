fs_file_type <- function(file_path) {
  out <- strsplit(
    basename(file_path),
    split = ".",
    fixed = TRUE
  )
  out <- vapply(out, utils::tail, character(1L), n = 1L)
  out <- tolower(out)
  return(out)
}

fs_file_path_normalise <- function(file_path) {
  normalizePath(
    path = file_path,
    winslash = "/",
    mustWork = FALSE
  )
}
