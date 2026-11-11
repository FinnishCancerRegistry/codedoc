cache_os_name__ <- function() {
  # taken from https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  return(unname(tolower(os)))
}

cache_dir_path__ <- function() {
  out <- fs_file_path_normalise__(switch(
    cache_os_name__(),
    linux = "~/.cache/R/codedoc",
    windows = sprintf(
      "C:/Users/%s/AppData/Local/R/cache/R/codedoc",
      Sys.getenv("USERNAME")
    ),
    macos = "~/Library/Caches/R/codedoc",
    stop(
      "internal error: could not detect operating system to determine ",
      "cache location. please report this to the maintainer."
    )
  ))
  return(out)
}

cache_file_path__ <- function() {
  sprintf("%s/cache.rds", cache_dir_path__())
}

cache_file_read__ <- function() {
  if (!file.exists(cache_file_path__())) {
    return(NULL)
  }
  readRDS(cache_file_path__())
}

cache_file_write__ <- function(
  text_file_df,
  codedoc_df
) {
  dbc::assert_prod_input_is_data_frame(text_file_df)
  dbc::assert_prod_input_is_data_frame(codedoc_df)
  saveRDS(
    list(text_file_df = text_file_df, codedoc_df = codedoc_df),
    cache_file_path__()
  )
}

cache_file_refresh__ <- function(
  text_file_df,
  codedoc_df
) {
  dbc::assert_prod_input_is_data_frame_with_required_names(
    text_file_df,
    required_names = c("path", "hash")
  )
  dbc::assert_prod_input_is_data_frame_with_required_names(
    codedoc_df,
    required_names = "text_file_path"
  )
  old <- cache_file_read__()
  replace_cache_text_file_paths <- intersect(
    old[["text_file_df"]][["path"]],
    text_file_df[["path"]]
  )
  new_text_file_df <- rbind(
    text_file_df,
    old[["text_file_df"]][
      !old[["text_file_df"]][["path"]] %in% replace_cache_text_file_paths
    ]
  )
  dbc::assert_prod_interim_all_are_TRUE(
    !duplicated(new_text_file_df)
  )
  new_codedoc_df <- rbind(
    codedoc_df,
    old[["codedoc_df"]][
      !old[["codedoc_df"]][["text_file_path"]] %in%
        replace_cache_text_file_paths
    ]
  )
  cache_file_write__(new_text_file_df, new_codedoc_df)
}

CACHE_RAM_ENV__ <- new.env()
CACHE_RAM_ENV__[["cache"]] <- NULL
cache_ram_refresh__ <- function() {
  CACHE_RAM_ENV__[["cache"]] <- cache_file_read__()
}
cache_ram_read__ <- function() {
  return(CACHE_RAM_ENV__[["cache"]])
}
cache_ram_write__ <- function(text_file_df, codedoc_df) {
  CACHE_RAM_ENV__[["cache"]] <- list(
    text_file_df = text_file_df,
    codedoc_df = codedoc_df
  )
  return(invisible(NULL))
}
cache_ram_use_is_possible__ <- function(
  text_file_paths
) {
  cached <- cache_ram_read__()
  return(
    inherits(cached, "list") &&
      all(c("text_file_df", "codedoc_df") %in% names(cached)) &&
      all(text_file_paths %in% cached[["text_file_df"]][["path"]])
  )
}
cache_ram_use_if_possible__ <- function(
  text_file_paths
) {
  if (cache_ram_use_is_possible__(text_file_paths)) {
    cached <- cache_ram_read__()
    return(cached[["codedoc_df"]][
      cached[["codedoc_df"]][["text_file_path"]] %in% text_file_paths,
    ])
  }
}
