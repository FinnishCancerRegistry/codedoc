

#' @name codedoc
#' @docType package
#' @title codedoc: Functions Document Your Code
#' @eval c(
#'   codedoc::codedoc_R_package_description("codedoc"),
#'   codedoc::codedoc_news_for_R_package()
#' )
NULL

# @codedoc_comment_block R_package_description(codedoc)
# `codedoc` is designed to aid your code documentation. Extract comments
# from the midst of your code and process them into documentation.
#
# # Recommended installation
#
# ```r
# devtools::install_github(
#   "FinnishCancerRegistry/codedoc",
#   ref = readline("enter latest tag on github: ")
# )
# ```
#
# # Example
# ```r
# @codedoc_insert_comment_block R_package_example(codedoc)
# ```
#
# @codedoc_comment_block R_package_description(codedoc)
