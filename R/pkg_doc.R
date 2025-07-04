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
    # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-03-19", "0.6.2")
    # Fixed `codedoc::pkg_doc_fun` extraction of param docs.
    # Multi-block docs could be mixed between parameters if they appeared in
    # a mixed order in text. Now they do not.
    # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-03-19", "0.6.2")
    extract_arg_list[["sort_by"]] <- c("key", "first_block_line")
    extract_arg_list[["detect_allowed_keys"]] <- function(x) {
      pkg_nm <- read.dcf(
        "DESCRIPTION",
        fields = "Package"
      )[1L, 1L]
      re_set <- pkg_doc_obj_regex_set__(pkg_nm, "package")
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

pkg_doc_obj_regex_set__ <- function(regex, type) {
  dbc::assert_prod_input_atom_is_in_set(
    type,
    set = c("package", "full")
  )
  patterns <- switch(
    type,
    full = c(
      # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-03", "0.10.4")
      # Add handling for examples. E.g. key `examples(mypkg::myfun)`.
      # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-03", "0.10.4")
      # @codedoc_comment_block codedoc:::pkg_doc_obj_regex_set__
      # - Collect comment block lines whose keys match one of:
      #   + `"^%s::"` for arguments --- but you should use roxygen
      #     block directly unless you have a specific reason.
      #   + `"^%s$"` for the description of the function.
      #     These go into either `Details` or section `Functions`, depending on
      #     `rdname`.
      #   + `"^return[(]%s[)]$"` for docs describing what the
      #     function returns. These appear under the section `Value` in the help
      #     page.
      #   + `"^examples[(]%s[)]$"` for the Examples section.
      # @codedoc_comment_block codedoc:::pkg_doc_obj_regex_set__
      param = "^%s::",
      descr = "^%s$",
      return = "^return[(]%s[)]$",
      examples = "^examples[(]%s[)]$"
      # dont need one for news because that is handled separately for now
    ),
    package = c(
      general = "^%s",
      news = "^news[(]\"%s",
      return = "^return[(]%s::",
      examples = "^examples[(]%s::"
    )
  )
  out <- sprintf(patterns, regex)
  names(out) <- names(patterns)
  return(out)
}

pkg_doc_obj_section_heads__ <- function(regex, has_rdname) {
  out <- list(
    param = NULL,
    descr = "@details",
    return = "@return",
    examples = "@examples"
  )
  if (has_rdname) {
    out <- list(
      param = NULL,
      descr = c(
        "@section Functions:",
        paste0("**", regex, "**")
      ),
      return = c(
        "@return",
        paste0("**", regex, "**")
      ),
      examples = c(
        "@examples",
        "",
        paste0("# ", regex)
      )
    )
  }
  return(out)
}

#' @eval codedoc:::pkg_doc_fun("codedoc::pkg_doc_obj", "pkg_doc")
pkg_doc_obj <- function(
  regex,
  rdname = NULL,
  text_file_paths = NULL
) {
  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-06-18", "0.10.0")
  # New function `codedoc::pkg_doc_obj`.
  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-06-18", "0.10.0")

  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-03", "0.10.2")
  # `codedoc::pkg_doc_obj` gains new arg `grepl_arg_list`.
  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-03", "0.10.2")
  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-03", "0.10.3")
  # `codedoc::pkg_doc_obj` argument `grepl_arg_list` handling improved:
  # - By default `perl = TRUE` but user can override this.
  # - If user supplies `fixed = TRUE`, we always set `perl = FALSE`.
  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-03", "0.10.3")
  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-04", "0.10.4")
  # `codedoc::pkg_doc_obj` argument `grepl_arg_list` removed because it was
  # not worth the trouble. To have a fixed expression, instead of passing
  # `fixed = TRUE` one can do `"\\Qmypkg::myobj\\E"`.
  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-04", "0.10.4")

  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-02", "0.10.1")
  # Expand `codedoc::pkg_doc_obj` allowed `regex` patterns.
  # Now `regex` can be any string that `grepl` can handle.
  # @codedoc_comment_block news("codedoc::pkg_doc_obj", "2025-07-02", "0.10.1")
  #' @param regex `[character]` (no default)
  #'
  #' Any regular expression accepted by `grepl` that contains `"::"` or `":::"`.
  #' E.g. `"mypkg::myfun"`.
  dbc::assert_is_character_nonNA_atom(regex)
  dbc::assert_match_regex(regex, grepl.arg.list = list(pattern = ":{2,3}"))

  grepl_arg_list <- list(
    pattern = regex,
    x = "test",
    perl = TRUE,
    fixed = FALSE
  )
  regex_test <- suppressWarnings(tryCatch(
    do.call(grepl, grepl_arg_list), error = function(e) e
  ))
  if (inherits(regex_test, "error")) {
    stop("Invalid `regex`: ", regex_test[["message"]])
  }
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
  if (!is.null(rdname)) {
    #' @param rdname `[NULL, character]` (default `NULL`)
    #'
    #' - `NULL`: No effect on generated docs.
    #' - `character`: Generate additional line `paste0("@rdname ", rdname)`.
    rdname_line <- paste0("@rdname ", rdname)
  }

  # @codedoc_comment_block codedoc::pkg_doc_obj
  # Document an object in an R package. Performs the following steps:
  #
  # - Calls `[codedoc::extract_keyed_comment_blocks]`.
  # @codedoc_comment_block codedoc::pkg_doc_obj
  #' @param text_file_paths `[NULL, character]` (default `NULL`)
  #'
  #' - `codedoc::pkg_doc_obj`:
  #'   Passed to `codedoc::codedoc_lines` and
  #'   `codedoc::codedoc_roxygen_news_by_version` calls.
  df <- pkg_doc_codedoc_df__(
    dir_path = getwd(),
    text_file_paths = text_file_paths
  )
  # @codedoc_comment_block codedoc::pkg_doc_obj
  # @codedoc_insert_comment_block codedoc:::pkg_doc_obj_regex_set__
  # @codedoc_comment_block codedoc::pkg_doc_obj
  regexes <- pkg_doc_obj_regex_set__(
    regex = regex,
    type = "full"
  )
  grepl_arg_list[["x"]] <- df[["key"]]
  section_heads <- local({
    # proper name in section. instead of showing e.g.
    # regex = "\\Qmypkg::[.myclass\\E" in docs, we want to show
    # "mypkg::[.myclass". so we figure out the name of the package and the
    # object.
    section_gal <- grepl_arg_list
    section_gal[["pattern"]] <- sprintf(
      "((^)|[(\"])%s(($)|[\")])",
      regex
    )
    keys <- df[["key"]][
      df[["key"]] == section_gal[["pattern"]] |
        do.call(grepl, section_gal)
    ]
    pkg_obj_nm_pairs <- unique(gsub("(^.*[(\"])|([)\"].*$)", "", keys))
    if (length(pkg_obj_nm_pairs) == 1) {
      regex <- pkg_obj_nm_pairs
    }
    pkg_doc_obj_section_heads__(
      regex = regex,
      has_rdname = !is.null(rdname)
    )
  })
  lines_by_section <- lapply(names(regexes), function(nm) {
    section_gal <- grepl_arg_list
    section_gal[["pattern"]] <- regexes[nm]
    lines <- unlist(df[["comment_block"]][do.call(grepl, section_gal)])
    if (length(lines) > 0) {
      head <- section_heads[[nm]]
      lines <- c(head, "", lines, "")
    }
    return(lines)
  })
  names(lines_by_section) <- names(regexes)

  # @codedoc_comment_block codedoc::pkg_doc_obj
  # - Call `[codedoc::codedoc_roxygen_news_by_version]` to extract news for
  #   the function, if any.
  # @codedoc_comment_block codedoc::pkg_doc_obj
  lines_by_section[["news"]] <- codedoc::codedoc_roxygen_news_by_version(
    regex,
    text_file_paths,
    extract_arg_list = list(detect_allowed_keys_grepl_arg_list = grepl_arg_list)
  )

  # @codedoc_comment_block codedoc::pkg_doc_obj
  # - If `regex` looks something like `xx::xx_make_column_xxxx`, matching
  #   regular expression
  #   `^[a-zA-Z0-9._-]+::[a-zA-Z0-9._-]+_make_column_[a-zA-Z0-9._-]+$`,
  #   its argument names which do not contain the words `dataset` or `setting`
  #   are assumed to be column names and are documented simply with
  #   ${gsub("`", "", deparse1(doc_fun_col_arg__("some.column")))}.
  #   This means you don't have to document such column arguments separately.
  # @codedoc_comment_block codedoc::pkg_doc_obj
  re_maker <- "^[a-zA-Z0-9._-]+::[a-zA-Z0-9._-]+_make_column_[a-zA-Z0-9._-]+$"
  if (grepl(re_maker, regex)) {
    obj <- tryCatch(
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
    if (is.function(obj)) {
      arg_nms <- names(formals(obj))
      col_arg_nms <- arg_nms[!grepl("(dataset)|(setting)", arg_nms)]
      lines_by_section[["param"]] <- c(
        lines_by_section[["param"]],
        unlist(lapply(col_arg_nms, doc_fun_col_arg__))
      )
    }
  }

  # @codedoc_comment_block codedoc::pkg_doc_obj
  # - Finally, collect all docs into one long string vector and add `@export`
  #   to the beginning of the vector.
  # @codedoc_comment_block codedoc::pkg_doc_obj
  lines <- c(
    "@export",
    rdname_line,
    "",
    unlist(lines_by_section, use.names = FALSE)
  )
  # @codedoc_comment_block return(codedoc::pkg_doc_obj)
  # Returns a `character` vector of lines for further processing by `roxygen2`.
  # @codedoc_comment_block return(codedoc::pkg_doc_obj)
  return(lines)
}

#' @eval codedoc:::pkg_doc_fun("codedoc::pkg_doc_fun", "pkg_doc")
pkg_doc_fun <- function(
  regex,
  rdname = NULL,
  text_file_paths = NULL
) {
  # @codedoc_comment_block codedoc::pkg_doc_obj
  # Document a function in an R package. Wrapper for
  # `codedoc::pkg_doc_fun` .
  # @codedoc_comment_block codedoc::pkg_doc_obj
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-03-10", "0.6.0")
  # New fun `codedoc::pkg_doc_fun`.
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-03-10", "0.6.0")
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-04-24", "0.9.0")
  # Fix for `codedoc::pkg_doc_fun` not being exported.
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-04-24", "0.9.0")
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-06-18", "0.10.0")
  # `codedoc::pkg_doc_fun` now a wrapper for `codedoc::pkg_doc_obj`.
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-06-18", "0.10.0")
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-07-03", "0.10.2")
  # `codedoc::pkg_doc_fun` gains new arg `grepl_arg_list`.
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-07-03", "0.10.2")
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-07-04", "0.10.4")
  # `codedoc::pkg_doc_fun` arg `grepl_arg_list` removed.
  # @codedoc_comment_block news("codedoc::pkg_doc_fun", "2025-07-04", "0.10.4")
  return(codedoc::pkg_doc_obj(
    regex = regex,
    rdname = rdname,
    text_file_paths = text_file_paths
  ))
}

#' @eval codedoc:::pkg_doc_fun("codedoc::pkg_doc_package_description", "pkg_doc")
pkg_doc_package_description <- function(
  text_file_paths = NULL,
  desc_arg_list = NULL,
  news_arg_list = NULL
) {
  # @codedoc_comment_block news("codedoc::pkg_doc_package_description", "2025-03-10", "0.6.0")
  # New fun `codedoc::pkg_doc_package_description`.
  # @codedoc_comment_block news("codedoc::pkg_doc_package_description", "2025-03-10", "0.6.0")
  # @codedoc_comment_block news("codedoc::pkg_doc_package_description", "2025-03-10", "0.6.0")
  # New fun `codedoc::pkg_doc_package_description`.
  # @codedoc_comment_block news("codedoc::pkg_doc_package_description", "2025-03-10", "0.6.0")
  # @codedoc_comment_block news("codedoc::pkg_doc_package_description", "2025-04-24", "0.9.0")
  # Fix for `codedoc::pkg_doc_package_description` not being exported.
  # @codedoc_comment_block news("codedoc::pkg_doc_package_description", "2025-04-24", "0.9.0")
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
