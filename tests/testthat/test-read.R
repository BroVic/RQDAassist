# test-read.R

test_that("validate input for 'read_transcript()'", {
  tmpdir <- tempdir()
  errNoFiles <- "argument \"docxfiles\" is missing, with no default"

  expect_error(read_transcript(NULL), errNoFiles)
  expect_error(read_transcript(tmpdir), errNoFiles)
  expect_error(read_transcript(tmpdir, "filenoexist.docx"),
               "File '' does not exist.")
  expect_error(read_transcript('fakedir'),
               "Directory 'fakedir' does not exist")
})



test_that("filepaths are returned", {
  fp <- read_transcript("data", "data/lorem.docx")
  onetxt <- normalizePath("data/lorem.txt", winslash = "/")

  expect_equal(fp, onetxt)

  file.remove(onetxt)
})
