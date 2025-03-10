
<!-- generated by R package codedoc; do not modify! -->

# Package `codedoc`


Allows generation of documentation from specifically
formatted code comments.

<!-- badges: start -->
[![R-CMD-check](https://github.com/FinnishCancerRegistry/codedoc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FinnishCancerRegistry/codedoc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Recommended installation

```r
devtools::install_github(
  "FinnishCancerRegistry/codedoc",
  ref = readline("enter latest tag on github: ")
)
```

# Example
```r
block_df <- codedoc::extract_keyed_comment_blocks(
  text_file_paths = codedoc::example_text_file_path("r_script.R")
)
print(block_df)
```



