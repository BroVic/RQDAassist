test_that("filepaths are returned", {
  fp <- read_transcript("data", "data/lorem.docx")
  onetxt <- normalizePath("data/lorem.txt")

  expect_equal(fp, onetxt)

  file.remove(onetxt)
})
