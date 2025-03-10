mb <- microbenchmark::microbenchmark(
  text_file_md5 <- vapply(
    X = text_file_paths,
    FUN = function(tfp) {
      digest::digest(
        file = tfp,
        algo = "md5"
      )
    },
    character(1L)
  ),
  times = 1e3L
)
