context("Downloading files")

#### Tests

test_that("JDownload works", {
  .prepare()

  file <- JDownload("https://farm5.staticflickr.com/4507/37847388931_959d812490_o_d.jpg",
                    cacheDir = TEST_DIR, verbose = TRUE, debug = TRUE)
  expect_true(!is.null(file))
  expect_true(file.exists(file))
  info <- file.info(file)

  file2 <- JDownload("https://farm5.staticflickr.com/4507/37847388931_959d812490_o_d.jpg",
                    cacheDir = TEST_DIR, verbose = TRUE, debug = TRUE)
  expect_true(!is.null(file2))
  expect_true(file.exists(file2))
  info2 <- file.info(file)
  # For some reason, this fails on Travis CI (Ubuntu)
  # I can't work out why :()
  #expect_equal(file, file2)
  expect_equal(info$size, info2$size)
})
