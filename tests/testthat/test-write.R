library(stringi)

test_that("FileList is created", {
  dir <- "data"
  fname1 <- "lipsum2.txt"
  fname2 <- "lipsum3.txt"
  file1 <- file.path(dir, fname1)
  file2 <- file.path(dir, fname2)
  cat(stri_rand_lipsum(2), file = file1, fill = TRUE)
  cat(stri_rand_lipsum(3), file = file2, fill = TRUE)
  files <- c(file1, file2)

  out <- make_FileList(files)

  expect_type(out, "list")
  expect_is(out, "list")
  expect_named(out, sub("\\.txt", "", basename(files)))
  expect_length(out, 2L)

  file.remove(files)
})
