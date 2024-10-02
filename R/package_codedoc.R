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

# @codedoc_comment_block news("codedoc", "2024-10-02", "0.5.0")
# Replaced `dbc::assert_is_data.frame*` -> `dbc::assert_is_data_frame*`.
# No effect on user, avoids a deprecated feature in `dbc`.
# @codedoc_comment_block news("codedoc", "2024-10-02", "0.5.0")
