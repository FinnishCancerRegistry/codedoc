#' @title Document R Package
#' @description
#' Tools to document an R package.
#' @name pkg_doc
NULL

pkg_doc_file_hash__ <- function(text_file_paths) {
  vapply(
    X = text_file_paths,
    FUN = function(tfp) {
      digest::digest(
        file = tfp,
        algo = "md5"
      )
    },
    character(1L)
  )
}

.__PKG_DOC_ENV__. <- new.env()
.__PKG_DOC_ENV__.[["codedoc_df"]] <- NULL
.__PKG_DOC_ENV__.[["text_file_df"]] <- NULL
pkg_doc_codedoc_df__ <- function(
  dir_path = ".",
  text_file_paths = NULL,
  extract_arg_list = NULL
) {
  stopifnot(
    dir.exists(dir_path),
    length(dir_path) == 1,

    inherits(extract_arg_list, c("NULL", "list")),

    inherits(text_file_paths, c("NULL", "character"))
  )
  if (!is.null(text_file_paths)) {
    text_file_paths <- normalizePath(
      text_file_paths,
      winslash = "/",
      mustWork = TRUE
    )
  }
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(dir_path)
  if (is.null(text_file_paths)) {
    text_file_paths <- text_file_paths_default__()
  }
  codedoc_df_is_valid <- !is.null(.__PKG_DOC_ENV__.[["codedoc_df"]])
  new_text_file_df <- data.frame(
    path = text_file_paths
  )
  if (codedoc_df_is_valid) {
    old_text_file_df <- .__PKG_DOC_ENV__.[["text_file_df"]]
    m <- match(new_text_file_df[["path"]], old_text_file_df[["path"]])
    codedoc_df_is_valid <- !any(is.na(m))
  }
  if (codedoc_df_is_valid) {
    new_text_file_df[["hash"]] <- pkg_doc_file_hash__(
      new_text_file_df[["path"]]
    )
    codedoc_df_is_valid <- identical(
      old_text_file_df[["hash"]][m],
      new_text_file_df[["hash"]]
    )
  }
  if (!codedoc_df_is_valid) {
    if (!"hash" %in% names(new_text_file_df)) {
      new_text_file_df[["hash"]] <- pkg_doc_file_hash__(
        new_text_file_df[["path"]]
      )
    }
    .__PKG_DOC_ENV__.[["text_file_df"]] <- new_text_file_df
    extract_arg_list <- as.list(extract_arg_list)
    extract_arg_list[["detect_allowed_keys"]] <- function(x) {
      pkg_nm <- read.dcf(
        "DESCRIPTION",
        fields = "Package"
      )[1L, 1L]
      re_set <- sprintf(
        c("^%s", "^news[(]\"%s", "^return[(]%s::"),
        pkg_nm
      )
      # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-03-14", "0.6.1")
      # `codedoc::pkg_doc_fun` now more robust when calling
      # `codedoc::extract_keyed_comment_blocks`.
      # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-03-14", "0.6.1")
      out <- rep(FALSE, length(x))
      for (re in re_set) {
        out <- out | grepl(re, x)
      }
      return(out)
    }
    extract_arg_list[["text_file_paths"]] <- text_file_paths
    .__PKG_DOC_ENV__.[["codedoc_df"]] <-
      do.call(codedoc::extract_keyed_comment_blocks, extract_arg_list,
              quote = TRUE)
  }
  return(.__PKG_DOC_ENV__.[["codedoc_df"]])
}

#' @eval codedoc::pkg_doc_fun("codedoc::pkg_doc_fun", "pkg_doc")
pkg_doc_fun <- function(
  regex,
  rdname = NULL,
  text_file_paths = NULL
) {
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-03-10", "0.6.0")
  # New fun `codedoc::pkg_doc_fun`.
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-03-10", "0.6.0")
  dbc::assert_match_regex(
    regex,
    grepl.arg.list = list(pattern = "^[a-zA-Z0-9._-]+::[a-zA-Z0-9._-]+$")
  )
  dbc::assert_is_one_of(
    rdname,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_atom)
  )
  dbc::assert_is_one_of(
    text_file_paths,
    funs = list(dbc::report_is_NULL,
                dbc::report_file_exists)
  )

  rdname_line <- NULL
  section_heads <- list(
    descr = "@details",
    return = "@return"
  )
  if (!is.null(rdname)) {
    #' @param rdname `[NULL, character]` (default `NULL`)
    #'
    #' - `NULL`: No effect on generated docs.
    #' - `character`: Generate additional line `paste0("@rdname ", rdname)`.
    rdname_line <- paste0("@rdname ", rdname)
    section_heads <- list(
      descr = c(
        "@section Functions:",
        paste0("**", regex, "**")
      ),
      return = c(
        "@return",
        paste0("**", regex, "**")
      )
    )
  }


  # @codedoc_comment_block codedoc::pkg_doc_fun
  # Document a function in an R package.
  #
  # Performs the following steps.
  # - Calls `[codedoc::extract_keyed_comment_blocks]`.
  # @codedoc_comment_block codedoc::pkg_doc_fun
  #' @param text_file_paths `[NULL, character]` (default `NULL`)
  #'
  #' - `codedoc::pkg_doc_fun`:
  #'   Passed to `codedoc::codedoc_lines` and
  #'   `codedoc::codedoc_roxygen_news_by_version` calls.
  #' @param regex `[character]` (no default)
  #'
  #' Must look like e.g. `"mypkg::myfun"`, where you write
  #' `codedoc` comment blocks with `mypkg::myfun` as/in the keys.
  #' This is used to detect which `codedoc` comment blocks to use in
  #' generating documents. Passed to `codedoc::codedoc_lines` as
  #' both `paste0("^", regex, "::")` and `paste0("^", regex, "$")` and to
  #' `codedoc::codedoc_roxygen_news_by_version` as-is.
  #' `paste0("^", regex, "::")` is meant for detecting `codedoc` comment
  #' blocks for arguments, e.g. `mypkgs::myfun::arg` --- but you should simply
  #' use `roxygen` blocks normally unless there is a special reason to do
  #' otherwise.
  df <- pkg_doc_codedoc_df__(
    dir_path = getwd(),
    text_file_paths = text_file_paths
  )
  # @codedoc_comment_block codedoc::pkg_doc_fun
  # - Collect comment block lines whose keys match one of:
  #   + `paste0("^", regex, "::")` for arguments --- but you should use roxygen
  #     block directly unless you have a specific reason.
  #   + `paste0("^", regex, "$")` for the description of the function.
  #     These go into either `Details` or section `Functions`, depending on
  #     `rdname`.
  #   + `paste0("^return[(]", regex, "[)]$")` for docs describing the what the
  #     function returns. These appear under the section `Value` in the help
  #     page.
  # @codedoc_comment_block codedoc::pkg_doc_fun
  regexes <- c(
    "param" = paste0("^", regex, "::"),
    "descr" = paste0("^", regex, "$"),
    "return" = paste0("^return[(]", regex, "[)]$")
  )
  lines_by_section <- lapply(names(regexes), function(nm) {
    regex <- regexes[nm]
    lines <- df[["comment_block"]][grepl(regex, df[["key"]], perl = TRUE)]
    if (length(lines) > 0) {
      head <- switch(
        nm,
        param = NULL,
        descr = section_heads[["descr"]],
        return = section_heads[["return"]]
      )
      lines <- c(head, "", lines, "")
    }
    return(lines)
  })
  names(lines_by_section) <- names(regexes)

  # @codedoc_comment_block codedoc::pkg_doc_fun
  # - Call `[codedoc::codedoc_roxygen_news_by_version]` to extract news for
  #   the function, if any.
  # @codedoc_comment_block codedoc::pkg_doc_fun
  lines_by_section[["news"]] <- codedoc::codedoc_roxygen_news_by_version(
    regex,
    text_file_paths
  )

  # @codedoc_comment_block codedoc::pkg_doc_fun
  # @codedoc_insert_comment_block explain(codedoc::pkg_doc_fun, maker)
  # @codedoc_comment_block codedoc::pkg_doc_fun

  # @codedoc_comment_block explain(codedoc::pkg_doc_fun, maker)
  # - If `regex` looks something like `xx::xx_make_column_xxxx`, matching
  #   regular expression
  #   `^[a-zA-Z0-9._-]+::[a-zA-Z0-9._-]+_make_column_[a-zA-Z0-9._-]+$`,
  #   its argument names which do not contain the words `dataset` or `setting`
  #   are assumed to be column names and are documented simply with
  #   ${gsub("`", "", deparse1(doc_fun_col_arg__("some.column")))}.
  #   This means you don't have to document such column arguments separately.
  # @codedoc_comment_block explain(codedoc::pkg_doc_fun, maker)
  re_maker <- "^[a-zA-Z0-9._-]+::[a-zA-Z0-9._-]+_make_column_[a-zA-Z0-9._-]+$"
  if (grepl(re_maker, regex)) {
    fun <- tryCatch(
      # replace :: -> ::: just in case it is not yet exported.
      eval(parse(text = gsub(":+", ":::", regex))),
      error = function(e) {
        stop(
          "`regex = ", deparse1(regex), "` looks like a `maker` function, ",
          "but that function could not be found with ",
          "`eval(parse(text = regex))`. Is the function name correct? This ",
          "was the error message: \"",
          paste0(e[["message"]], collapse = " "), "\""
        )
      }
    )
    arg_nms <- names(formals(fun))
    col_arg_nms <- arg_nms[!grepl("(dataset)|(setting)", arg_nms)]
    lines_by_section[["param"]] <- c(
      lines_by_section[["param"]],
      unlist(lapply(col_arg_nms, doc_fun_col_arg__))
    )
  }

  # @codedoc_comment_block codedoc::pkg_doc_fun
  # - Finally, collect all docs into one long string vector and add `@export`
  #   to the beginning of the vector.
  # @codedoc_comment_block codedoc::pkg_doc_fun
  lines <- c(
    "@export",
    rdname_line,
    "",
    unlist(lines_by_section, use.names = FALSE)
  )
  # @codedoc_comment_block return(codedoc::pkg_doc_fun)
  # Returns a `character` vector of lines for further processing by `roxygen2`.
  # @codedoc_comment_block return(codedoc::pkg_doc_fun)
  return(lines)
}

#' @eval codedoc::pkg_doc_fun("codedoc::pkg_doc_package_description", "pkg_doc")
pkg_doc_package_description <- function(
  text_file_paths = NULL,
  desc_arg_list = NULL,
  news_arg_list = NULL
) {
  # @codedoc_comment_block news("codedoc::pkg_doc_package_description", "2025-03-10", "0.6.0")
  # New fun `codedoc::pkg_doc_package_description`.
  # @codedoc_comment_block news("codedoc::pkg_doc_package_description", "2025-03-10", "0.6.0")
  # @codedoc_comment_block codedoc::pkg_doc_package_description
  # Calls `codedoc::codedoc_R_package_description` and
  # `codedoc::codedoc_news_for_R_package`.
  # @codedoc_comment_block codedoc::pkg_doc_package_description

  #' @param desc_arg_list `[NULL, list]` (default `NULL`)
  #'
  #' List of arguments passed to
  #' `codedoc::codedoc_R_package_description`.
  stopifnot(inherits(desc_arg_list, c("NULL", "list")))
  desc_arg_list <- as.list(desc_arg_list)
  if (!"R_package_name" %in% names(desc_arg_list)) {
    desc_arg_list[["R_package_name"]] <- read.dcf(
      file = "DESCRIPTION",
      fields = "Package"
    )[1L, 1L]
  }

  #' @param news_arg_list `[NULL, list]` (default `NULL`)
  #'
  #' List of arguments passed to
  #' `codedoc::codedoc_news_for_R_package`.
  stopifnot(inherits(desc_arg_list, c("NULL", "list")))
  news_arg_list <- as.list(news_arg_list)

  #' @param text_file_paths `[NULL, character]` (default `NULL`)
  #'
  #' - `codedoc::pkg_doc_package_description`:
  #'   Overrides `text_file_paths` passed to
  #'   `codedoc::codedoc_R_package_description` and
  #'   `codedoc::codedoc_news_for_R_package`.
  desc_arg_list[["text_file_paths"]] <- text_file_paths
  news_arg_list[["text_file_paths"]] <- text_file_paths

  desc <- do.call(codedoc::codedoc_R_package_description, desc_arg_list,
                  quote = TRUE)
  news <- do.call(codedoc::codedoc_news_for_R_package, news_arg_list,
                  quote = TRUE)
  # @codedoc_comment_block return(codedoc::pkg_doc_package_description)
  # Returns a `character` vector, the combined outputs of the functions called
  # by this one.
  # @codedoc_comment_block return(codedoc::pkg_doc_package_description)
  c(desc, "", news)
}

doc_fun_col_arg__ <- function(arg.nm) {
  stopifnot(
    is.character(arg.nm),
    !is.na(arg.nm),
    length(arg.nm) == 1,
    !grepl("_", arg.nm)
  )
  c(
    sprintf("@param %s (no default)", arg.nm),
    "",
    sprintf("Column `%s` values.", gsub("[.]", "_", arg.nm))
  )
}
