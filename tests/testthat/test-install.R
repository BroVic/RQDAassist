test_that("input is validated", {
  for (fn in c("install", "install_rgtk2_and_deps")) {
    expect_error(do.call(fn, list(type = 999)),
                 "'arg' must be NULL or a character vector")
    expect_error(do.call(fn, list(type = "999")),
                 "'arg' should be one of \"binary\", \"source\"",
                 fixed = TRUE)
    expect_error(do.call(fn, list(verbose = 999)),
                 "'verbose' must be logical vector")
  }
})
