fs_file_extension__ <- function(file_path) {
  out <- strsplit(
    basename(file_path),
    split = ".",
    fixed = TRUE
  )
  out <- vapply(out, utils::tail, character(1L), n = 1L)
  return(out)
}

fs_file_type__ <- function(file_path) {
  out <- tolower(fs_file_extension__(file_path))
  return(out)
}

fs_file_path_normalise__ <- function(file_path) {
  normalizePath(
    path = file_path,
    winslash = "/",
    mustWork = FALSE
  )
}
