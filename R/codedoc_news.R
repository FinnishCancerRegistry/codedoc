
#' @title News from `codedoc` Comment Blocks
#' @description
#' These functions can be used to collate news about a versioned project such
#' as an R package.
#' @name codedoc_news
#' @eval c(
#'   "@section Functions:",
#'   codedoc::codedoc_lines(
#'     detect_allowed_keys = "^codedoc::codedoc_.*news.+",
#'     text_file_paths = "R/codedoc_news.R"
#'   ),
#'   "",
#'   codedoc::codedoc_roxygen_news_by_version(
#'     detect_allowed_keys = "codedoc::codedoc_.*news.+",
#'     text_file_paths = "R/codedoc_news.R"
#'   )
#' )
NULL

#' @export
#' @rdname codedoc_news
#' @eval codedoc::codedoc_lines(
#'   detect_allowed_keys = "codedoc::codedoc_news_df::",
#'   text_file_paths = "R/codedoc_news.R"
#' )
codedoc_news_df <- function(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::codedoc_news_df", "2022-02-17", "0.2.15")
  # New exported function `[codedoc::codedoc_news_df]`.
  # @codedoc_comment_block news("codedoc::codedoc_news_df", "2022-02-17", "0.2.15")

  # @codedoc_comment_block codedoc::codedoc_news_df
  #
  # ## `codedoc::codedoc_news_df`
  #
  # `codedoc::codedoc_news_df` performs the following steps:
  #
  # - `[codedoc::extract_keyed_comment_blocks]` is called; allowed keys must
  #   begin with "news(`
  #
  # @codedoc_comment_block codedoc::codedoc_news_df
  dbc::assert_is_assertion_type(
    assertion_type,
    call = match.call(),
    assertion_type = assertion_type
  )

  # @codedoc_comment_block codedoc::codedoc_news_df::detect_allowed_keys
  # @param detect_allowed_keys `[character, function]` (default `".*"`)
  #
  # - `character`: use `paste0("^news[(]\"", detect_allowed_keys, "\"")`
  # - `function`: use this function
  #
  # This argument is passed to `[codedoc::extract_keyed_comment_blocks]` after
  # any pre-processing.
  # @codedoc_comment_block codedoc::codedoc_news_df::detect_allowed_keys
  if (is.character(detect_allowed_keys)) {
    detect_allowed_keys <- paste0("^news[(]\"", detect_allowed_keys, "\"")
  }

  # @codedoc_comment_block codedoc::codedoc_news_df::extract_arg_list
  # @param extract_arg_list `[NULL, list]` (default `NULL`)
  #
  # Arguments passed to `[codedoc::extract_keyed_comment_blocks]`.
  # @codedoc_comment_block codedoc::codedoc_news_df::extract_arg_list
  dbc::assert_is_one_of(
    extract_arg_list,
    funs = list(dbc::report_is_NULL, dbc::report_is_list),
    assertion_type = assertion_type
  )
  extract_arg_list <- as.list(extract_arg_list)
  extract_arg_list[["detect_allowed_keys"]] <- detect_allowed_keys
  # @codedoc_comment_block codedoc::codedoc_news_df::text_file_paths
  # @param text_file_paths `[NULL, character]` (default `NULL`)
  #
  # This argument is passed to `[codedoc::extract_keyed_comment_blocks]`
  # as-is.
  # @codedoc_comment_block codedoc::codedoc_news_df::text_file_paths
  assert_arg_text_file_paths(text_file_paths)
  extract_arg_list[["text_file_paths"]] <- text_file_paths
  news_df <- do.call(codedoc::extract_keyed_comment_blocks, extract_arg_list,
                     quote = TRUE)
  # @codedoc_comment_block codedoc::codedoc_news_df
  #
  # - Each collected key is parsed, yielding parsed language object, e.g.
  #   `quote(news("codedoc::codedoc_news_df", "2022-02-03", "0.3.35"))`;
  #   the object name, date, and project version are collected from this parsed
  #   expression
  #
  # @codedoc_comment_block codedoc::codedoc_news_df
  key_exprs <- lapply(news_df[["key"]], function(key) {
    parse(text = key)[[1L]]
  })
  e <- environment()
  new_col_nms <- c("object_name", "date", "version")
  lapply(seq_along(new_col_nms), function(i) {
    e[["news_df"]][[new_col_nms[i]]] <- vapply(
      key_exprs,
      function(key_expr) {
        if (length(key_expr) >= i + 1L) {
          string <- key_expr[[i + 1L]]
        } else {
          string <- NA_character_
        }
        return(string)
      },
      character(1L)
    )
    NULL
  })
  stopifnot(
    !is.na(news_df[["object_name"]]),
    !is.na(as.Date(news_df[["date"]])),
    !is.na(numeric_version(news_df[["version"]]))
  )
  news_df <- news_df[
    order(news_df[["version"]], news_df[["object_name"]],
          decreasing = c(TRUE, FALSE), method = "radix"),

  ]
  # @codedoc_comment_block codedoc::codedoc_news_df
  #
  # - A `data.frame` is returned that contains the usual codedoc columns and
  #   the additional columns `"object_name"`, `"date"`, and `"version"`
  #
  # @codedoc_comment_block codedoc::codedoc_news_df
  return(news_df)
}

assert_arg_df <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = "input"
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  dbc::assert_is_data.frame_with_required_names(
    x,
    x_nm = x_nm,
    call = call,
    required_names = c("version", "comment_block"),
    assertion_type = assertion_type
  )
  dbc::assert_is_list(
    x$comment_block,
    x_nm = paste0(x_nm, "$comment_block"),
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_is_nonNA(
    as.numeric_version(x$version),
    x_nm = paste0("as.numeric_version(", x_nm, "$version)"),
    call = call,
    assertion_type = assertion_type
  )
}

#' @export
#' @rdname codedoc_news
#' @param df `[data.frame]` (no default)
#'
#' `data.frame` as produced by `[codedoc::codedoc_news_df]`.
codedoc_news_df_to_roxygen_news_by_version <- function(
  df,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_roxygen_news_by_version", "2022-02-17", "0.2.15")
  #
  # New exported function `codedoc::codedoc_news_df_to_roxygen_news_by_version`.
  #
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_roxygen_news_by_version", "2022-02-17", "0.2.15")

  assert_arg_df(df, assertion_type = assertion_type)

  # @codedoc_comment_block codedoc::codedoc_news_df_to_roxygen_news_by_version
  #
  # ## `codedoc::codedoc_news_df_to_roxygen_news_by_version`
  #
  # `codedoc::codedoc_news_df_to_roxygen_news_by_version` simply collates
  # one long vector of strings, where different version numbers have their own
  # roxygen section with
  # `paste0("@section News for version ", df[["version"]][i], ":")`.
  #
  # @codedoc_comment_block codedoc::codedoc_news_df_to_roxygen_news_by_version
  version_set <- as.numeric_version(unique(df[["version"]]))
  version_set <- as.character(sort(version_set))
  unlist(lapply(version_set, function(version) {
    row_no_set <- which(df[["version"]] == version)
    c(
      paste0("@section News for version ", version, ":"),
      "",
      unlist(lapply(df[["comment_block"]][row_no_set], function(cb) {
        c("", cb, "")
      })),
      ""
    )
  }))
}

#' @export
#' @rdname codedoc_news
codedoc_roxygen_news_by_version <- function(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::codedoc_roxygen_news_by_version", "2022-02-17", "0.2.15")
  #
  # New exported function `codedoc::codedoc_roxygen_news_by_version`.
  #
  # @codedoc_comment_block news("codedoc::codedoc_roxygen_news_by_version", "2022-02-17", "0.2.15")

  # @codedoc_comment_block codedoc::codedoc_roxygen_news_by_version
  #
  # ## `codedoc::codedoc_roxygen_news_by_version`
  #
  # `codedoc::codedoc_roxygen_news_by_version` runs
  # `codedoc::codedoc_news_df` and
  # `codedoc::codedoc_news_df_to_roxygen_news_by_version`.
  #
  # @codedoc_comment_block codedoc::codedoc_roxygen_news_by_version
  df <- codedoc::codedoc_news_df(
    detect_allowed_keys = detect_allowed_keys,
    text_file_paths = text_file_paths,
    extract_arg_list = extract_arg_list,
    assertion_type = assertion_type
  )
  codedoc::codedoc_news_df_to_roxygen_news_by_version(
    df,
    assertion_type = "input"
  )
}

#' @export
#' @rdname codedoc_news
codedoc_news_df_to_roxygen_news_by_object_name <- function(
  df,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_roxygen_news_by_object_name", "2022-02-17", "0.2.15")
  #
  # New exported function `codedoc::codedoc_news_df_to_roxygen_news_by_object_name`.
  #
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_roxygen_news_by_object_name", "2022-02-17", "0.2.15")

  # @codedoc_comment_block codedoc::codedoc_news_df_to_roxygen_news_by_object_name
  #
  # ## `codedoc::codedoc_news_df_to_roxygen_news_by_object_name`
  #
  # `codedoc::codedoc_news_df_to_roxygen_news_by_object_name` simply collates
  # one long vector of strings, where different objects have their own
  # roxygen section. Version and date are mentioned for each separate item under
  # the section.
  #
  # @codedoc_comment_block codedoc::codedoc_news_df_to_roxygen_news_by_object_name

  assert_arg_df(df, assertion_type = assertion_type)
  dbc::assert_is_Date_nonNA_vector(
    as.Date(df$date),
    assertion_type = assertion_type
  )

  object_name_set <- unique(df[["object_name"]])
  unlist(lapply(object_name_set, function(obj_nm) {
    row_no_set <- which(df[["object_name"]] == obj_nm)
    if (grepl(":::", obj_nm)) {
      pkg_obj <- strsplit(obj_nm, ":::")[[1L]]
      pkg_nm <- pkg_obj[1L]
      bare_obj_nm <- pkg_obj[2L]

      title <- paste0(
        "@section News for package ", deparse(pkg_nm), " unexported object ",
        deparse(bare_obj_nm), ":"
      )
    } else if (grepl("::", obj_nm)) {
      pkg_obj <- strsplit(obj_nm, "::")[[1L]]
      pkg_nm <- pkg_obj[1L]
      bare_obj_nm <- pkg_obj[2L]

      title <- paste0(
        "@section News for package ", deparse(pkg_nm), " object ",
        deparse(bare_obj_nm), ":"
      )
    } else {
      title <- paste0("@section News for object ", deparse(obj_nm), ":")
    }
    c(
      title,
      "",
      unlist(lapply(row_no_set, function(i) {
        version_date <- paste0(
          "**", df[["version"]][i], " @ ", df[["date"]][i], "**"
        )
        c(
          version_date,
          "",
          df[["comment_block"]][[i]],
          ""
        )
      }))
    )
  }))
}


#' @export
#' @rdname codedoc_news
codedoc_roxygen_news_by_object_name <- function(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_roxygen_news_by_object_name", "2022-02-17", "0.2.15")
  #
  # New exported function `codedoc::codedoc_roxygen_news_by_object_name`.
  #
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_roxygen_news_by_object_name", "2022-02-17", "0.2.15")

  # @codedoc_comment_block codedoc::codedoc_roxygen_news_by_object_name
  #
  # ## `codedoc::codedoc_roxygen_news_by_object_name`
  #
  # `codedoc::codedoc_roxygen_news_by_object_name` runs
  # `codedoc::codedoc_news_df` and
  # `codedoc::codedoc_news_df_to_roxygen_news_by_object_name`.
  #
  # @codedoc_comment_block codedoc::codedoc_roxygen_news_by_object_name

  df <- codedoc::codedoc_news_df(
    detect_allowed_keys = detect_allowed_keys,
    text_file_paths = text_file_paths,
    extract_arg_list = extract_arg_list,
    assertion_type = assertion_type
  )
  codedoc::codedoc_news_df_to_roxygen_news_by_object_name(
    df, assertion_type = "input"
  )
}


#' @export
#' @rdname codedoc_news
codedoc_news_df_to_markdown_news <- function(
  df,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_markdown_news", "2022-02-17", "0.2.15")
  #
  # New exported function `codedoc::codedoc_news_df_to_markdown_news`.
  #
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_markdown_news", "2022-02-17", "0.2.15")

  # @codedoc_comment_block codedoc::codedoc_news_df_to_markdown_news
  #
  # ## `codedoc::codedoc_news_df_to_markdown_news`
  #
  # `codedoc::codedoc_news_df_to_markdown_news` simply collates
  # one long vector of strings, where each version has a level two markdown
  # title (e.g. "## 0.3.35") and each object a level three title
  # (e.g. "### codedoc::codedoc_news_df_to_markdown_news")
  #
  # @codedoc_comment_block codedoc::codedoc_news_df_to_markdown_news

  assert_arg_df(df)
  version_set <- sort(numeric_version(unique(df[["version"]])),
                      decreasing = TRUE)
  version_set <- setdiff(as.character(version_set), NA_character_)
  unlist(lapply(seq_along(version_set), function(version_set_pos) {
    version <- version_set[version_set_pos]
    row_no_set <- which(df[["version"]] == version)

    c(
      paste0("## News for version ", version),
      "",
      unlist(lapply(row_no_set, function(row_no) {
        c(
          paste0("### Object ", df[["object_name"]][row_no], " news:"),
          "",
          df[["comment_block"]][[row_no]],
          ""
        )
      })),
      ""
    )
  }))
}


#' @export
#' @rdname codedoc_news
codedoc_markdown_news <- function(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::codedoc_markdown_news", "2022-02-17", "0.2.15")
  #
  # New exported function `codedoc::codedoc_markdown_news`.
  #
  # @codedoc_comment_block news("codedoc::codedoc_markdown_news", "2022-02-17", "0.2.15")

  # @codedoc_comment_block codedoc::codedoc_markdown_news
  #
  # ## `codedoc::codedoc_markdown_news`
  #
  # `codedoc::codedoc_markdown_news` runs
  # `codedoc::codedoc_news_df` and
  # `codedoc::codedoc_news_df_to_markdown_news`.
  #
  # @codedoc_comment_block codedoc::codedoc_markdown_news

  df <- codedoc::codedoc_news_df(
    detect_allowed_keys = detect_allowed_keys,
    text_file_paths = text_file_paths,
    extract_arg_list = extract_arg_list,
    assertion_type = assertion_type
  )
  codedoc::codedoc_news_df_to_markdown_news(
    df, assertion_type = "input"
  )
}


#' @export
#' @rdname codedoc_news
codedoc_news_for_R_package <- function(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_markdown_news", "2022-02-17", "0.2.15")
  #
  # New exported function `codedoc::codedoc_news_df_to_markdown_news`.
  #
  # @codedoc_comment_block news("codedoc::codedoc_news_df_to_markdown_news", "2022-02-17", "0.2.15")

  # @codedoc_comment_block codedoc::codedoc_news_for_R_package
  #
  # ## `codedoc::codedoc_news_for_R_package`
  #
  # `codedoc::codedoc_news_for_R_package` calls `codedoc::codedoc_news_df`
  # and `codedoc::codedoc_news_df_to_markdown_news` to collate news.
  # The R package news file `NEWS.md` is created/written over.
  # Output is the collated news, a string vector, prepended with
  # `"@section News:"`
  #
  # @codedoc_comment_block codedoc::codedoc_news_for_R_package

  df <- codedoc::codedoc_news_df(
    detect_allowed_keys = detect_allowed_keys,
    text_file_paths = text_file_paths,
    extract_arg_list = extract_arg_list,
    assertion_type = assertion_type
  )
  lines <- codedoc::codedoc_news_df_to_markdown_news(
    df, assertion_type = "input"
  )
  writeLines(lines, "NEWS.md")
  c(
    "@section News:",
    "",
    lines
  )
}
