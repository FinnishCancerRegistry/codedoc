#' @eval c(
#'   codedoc::codedoc_R_package_description("codedoc"),
#'   codedoc::codedoc_news_for_R_package()
#' )
#' @keywords internal
"_PACKAGE"

# @codedoc_comment_block R_package_description(codedoc)
# ${desc::desc_get_field("Description")}
#
# <!-- badges: start -->
# [![R-CMD-check](https://github.com/FinnishCancerRegistry/codedoc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FinnishCancerRegistry/codedoc/actions/workflows/R-CMD-check.yaml)
# <!-- badges: end -->
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
