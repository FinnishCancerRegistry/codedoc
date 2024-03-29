% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codedoc_news.R
\name{codedoc_news}
\alias{codedoc_news}
\alias{codedoc_news_df}
\alias{codedoc_news_df_to_roxygen_news_by_version}
\alias{codedoc_roxygen_news_by_version}
\alias{codedoc_news_df_to_roxygen_news_by_object_name}
\alias{codedoc_roxygen_news_by_object_name}
\alias{codedoc_news_df_to_markdown_news}
\alias{codedoc_markdown_news}
\alias{codedoc_news_for_R_package}
\title{News from \code{codedoc} Comment Blocks}
\usage{
codedoc_news_df(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
)

codedoc_news_df_to_roxygen_news_by_version(df, assertion_type = "input")

codedoc_roxygen_news_by_version(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
)

codedoc_news_df_to_roxygen_news_by_object_name(df, assertion_type = "input")

codedoc_roxygen_news_by_object_name(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
)

codedoc_news_df_to_markdown_news(df, assertion_type = "input")

codedoc_markdown_news(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
)

codedoc_news_for_R_package(
  detect_allowed_keys = ".*",
  text_file_paths = NULL,
  extract_arg_list = NULL,
  assertion_type = "input"
)
}
\arguments{
\item{detect_allowed_keys}{\verb{[character, function]} (default \code{".*"})
\itemize{
\item \code{character}: use \verb{paste0("^news[(]\\"", detect_allowed_keys, "\\"")}
\item \code{function}: use this function
}

This argument is passed to \verb{[codedoc::extract_keyed_comment_blocks]} after
any pre-processing.}

\item{text_file_paths}{\verb{[NULL, character]} (default \code{NULL})

This argument is passed to \verb{[codedoc::extract_keyed_comment_blocks]}
as-is.}

\item{extract_arg_list}{\verb{[NULL, list]} (default \code{NULL})

Arguments passed to \verb{[codedoc::extract_keyed_comment_blocks]}.}

\item{assertion_type}{see \verb{[dbc::report_to_assertion]}}

\item{df}{\verb{[data.frame]} (no default)

\code{data.frame} as produced by \verb{[codedoc::codedoc_news_df]}.}
}
\description{
These functions can be used to collate news about a versioned project such
as an R package.
}
\section{Functions}{

\subsection{\code{codedoc::codedoc_news_df}}{

\code{codedoc::codedoc_news_df} performs the following steps:
\itemize{
\item \verb{[codedoc::extract_keyed_comment_blocks]} is called; allowed keys must
begin with "news(`
}
}
}

\section{News for version 0.4.0}{


\code{codedoc::codedoc_news_df_to_markdown_news} title format changed from
\verb{### Object x news:} to simply \verb{### x}.
}

\section{News for version 0.3.1}{


\verb{[codedoc::codedoc_news_df_to_markdown_news]} now includes a markdown
comment to notify that its output was generated by \code{codedoc}.

\verb{[codedoc::codedoc_news_df_to_roxygen_news_by_version]}:
The order of versions in is now descending instead of ascending.

\verb{[codedoc::codedoc_news_for_R_package]} avoids markdown comments in
output (but they are included in \code{NEWS.md}).
}

\section{News for version 0.2.15}{


New exported function \code{codedoc::codedoc_markdown_news}.

New exported function \verb{[codedoc::codedoc_news_df]}.

New exported function \code{codedoc::codedoc_news_df_to_markdown_news}.

New exported function \code{codedoc::codedoc_news_df_to_roxygen_news_by_object_name}.

New exported function \code{codedoc::codedoc_roxygen_news_by_object_name}.

New exported function \code{codedoc::codedoc_news_df_to_roxygen_news_by_version}.

New exported function \code{codedoc::codedoc_roxygen_news_by_version}.
}

