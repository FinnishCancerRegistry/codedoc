cache_dir_path__ <- function() {
  out <- fs_file_path_normalise__(tools::R_user_dir("codedoc", "cache"))
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
cache_ram_use_if_possible__ <- function(
  text_file_df
) {
  cached <- cache_ram_read__()
  intersect_text_file_df <- merge(
    x = text_file_df[, c("path", "hash")],
    y = cached[["text_file_df"]][, c("path", "hash")],
    by = c("path", "hash"),
    all.x = FALSE,
    all.y = FALSE
  )
  if (nrow(intersect_text_file_df) == nrow(text_file_df)) {
    return(cached[["codedoc_df"]][
      cached[["codedoc_df"]][["text_file_path"]] %in% text_file_df[["path"]],
    ])
  } else {
    return(NULL)
  }
}

cache_refresh__ <- function(
  text_file_df,
  codedoc_df
) {
  cache_file_refresh__(text_file_df = text_file_df, codedoc_df = codedoc_df)
  cache_ram_refresh__()
}
cache_text_file_path_hash__ <- function(text_file_path) {
  digest::digest(
    file = text_file_path,
    algo = "md5"
  )
}
cache_text_df__ <- function(
  text_file_paths
) {
  hashes <- vapply(text_file_paths, cache_text_file_path_hash__, character(1L))
  return(data.frame(path = text_file_paths, hash = hashes))
}
