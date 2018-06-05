context("Plot to file")
#library(JUtils)
library(png)
library(tiff)

.mmToInches <- function(x) (x / 25.4)
.cmToInches <- function(x) (x / 2.54)

TEST_DIR <- "files"
# Returns path of test file with the specified name
tf <- function(name) file.path(TEST_DIR, name)
cleanupTestFiles <- function() {
  do.call(file.remove, list(list.files(TEST_DIR, full.names = TRUE)))
}

plotWigglyLines <- function(cex = 1, lwd = 2, ...) {
  set.seed(1)
  plot(rnorm(20), type = "l", main = "Wiggly Lines", ylim = c(-2.2, 3), lwd = lwd, cex = cex, cex.main = cex, ...)
  lines(rnorm(20), col = "red", lty = 2, lwd = lwd, ...)
  legend("topright", legend = c("A black line", "A dashed red line"), lty = c(1, 2), col = c("black", "red"), lwd = lwd, ...)
}

.prepare <- function() {
  if (!dir.exists(TEST_DIR))
    dir.create(TEST_DIR)
  # Delete test files now and after completion
  cleanupTestFiles()
  # Completion is when the caller exits, not when this function
  #do.call("on.exit", list(substitute(on.exit(cleanupTestFiles())), add=TRUE), envir=parent.frame())
}


#### Tests

test_that("png plotting", {
  .prepare()

  width <- 180
  height <- 120
  units <- "mm"
  res <- 72 # in ppi

  .mmToPixels <- function(x) floor(.mmToInches(x) * res)

  img <- tf("test.png")
  JPlotToPNG(img, plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists(img))
  png <- readPNG(img, native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], .mmToPixels(width))
  expect_equal(info$dim[2], .mmToPixels(height))
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("png plotting fn", {
  .prepare()

  width <- 180
  height <- 120
  units <- "mm"
  res <- 72 # in ppi

  .mmToPixels <- function(x) floor(.mmToInches(x) * res)

  img <- tf("test.png")
  JPlotToPNG(img, plotWigglyLines, width = width, height = height, res = res, units = units)
  expect_true(file.exists(img))
  png <- readPNG(img, native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], .mmToPixels(width))
  expect_equal(info$dim[2], .mmToPixels(height))
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("png plotting in pixels", {
  .prepare()
  width <- 800
  height <- 600
  units <- "px"
  res <- 72 # in ppi

  .mmToPixels <- function(x) floor(.mmToInches(x) * res)

  img <- tf("test.png")
  JPlotToPNG(img, plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists(img))
  png <- readPNG(img, native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], width)
  expect_equal(info$dim[2], height)
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("png plotting in cm", {
  .prepare()
  width <- 18
  height <- 12
  units <- "cm"
  res <- 100 # in ppi

  .cmToPixels <- function(x) floor(.cmToInches(x) * res)

  img <- tf("test.png")
  JPlotToPNG(img, plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists(img))
  png <- readPNG(img, native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], .cmToPixels(width))
  expect_equal(info$dim[2], .cmToPixels(height))
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("png plotting in mm", {
  .prepare()
  width <- 180
  res <- 300 # in ppi
  # Default height, aspect ratio and units
  expectHeight <- 180 / (3 / 2)

  .mmToPixels <- function(x) floor(.mmToInches(x) * res)

  img <- tf("test.png")
  JPlotToPNG(img, plotWigglyLines(), width = width, res = res)
  expect_true(file.exists(img))
  png <- readPNG(img, native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], .mmToPixels(width))
  expect_equal(info$dim[2], .mmToPixels(expectHeight))
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("png plotting in in", {
  .prepare()
  width <- 7
  height <- 4.7
  units <- "in"
  res <- 120 # in ppi

  .inToPixels <- function(x) floor(x * res)

  img <- tf("test.png")
  JPlotToPNG(img, plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists(img))
  png <- readPNG(img, native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], .inToPixels(width))
  expect_equal(info$dim[2], .inToPixels(height))
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("tiff plotting", {
  .prepare()
  width <- 180
  height <- 120
  units <- "mm"
  res <- 72 # in ppi

  .mmToPixels <- function(x) floor(.mmToInches(x) * res)

  img <- tf("test.tif")
  JPlotToTIFF(img, plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists(img))
  tiff <- readTIFF(img, native = TRUE, info = TRUE)
  expect_equal(attr(tiff, "dim")[2], .mmToPixels(width))
  expect_equal(attr(tiff, "dim")[1], .mmToPixels(height))
  expect_equal(attr(tiff, "x.resolution"), res, tolerance = .1)
  expect_equal(attr(tiff, "y.resolution"), res, tolerance = .1)
})

test_that("pdf plotting", {
  .prepare()
  width <- 180
  height <- 120
  units <- "mm"

  img <- tf("test.pdf")
  JPlotToPDF(img, plotWigglyLines(), width = width, height = height, units = units)
  expect_true(file.exists(img))
  lines <- readLines(img, n=10)
})

test_that("eps plotting", {
  .prepare()
  width <- 180
  height <- 120
  units <- "mm"

  img <- tf("test.eps")
  JPlotToEPS(img, plotWigglyLines(), width = width, height = height, units = units)
  expect_true(file.exists(img))
  lines <- readLines(img, n=10)
})

test_that("General plotting", {
  .prepare()
  width <- 180
  height <- 120
  units <- "mm"

  img <- tf("test.eps")
  JPlotToFile(img, plotWigglyLines(), width = width, height = height, units = units)
  expect_true(file.exists(img))
  lines <- readLines(img, n=10)
})

test_that("Multiple plots expr", {
  .prepare()
  width <- 180
  height <- 120
  units <- "mm"

  imgs <- c(tf("test.png"), tf("test.pdf"), tf("test.eps"), tf("test.tif"))
  JPlotToFile(imgs, plotWigglyLines(), width = width, height = height, units = units)
  for (img in imgs) {
    expect_true(file.exists(img), info = img)
  }
  lines <- readLines(imgs[1], n=10)
})

test_that("Multiple plots fn", {
  .prepare()
  width <- 180
  height <- 120
  units <- "mm"

  imgs <- c(tf("test.png"), tf("test.pdf"), tf("test.eps"), tf("test.tif"))
  JPlotToFile(imgs, plotWigglyLines, width = width, height = height, units = units)
  for (img in imgs) {
    expect_true(file.exists(img), info = img)
  }
  lines <- readLines(imgs[1], n=10)
})

test_that("Height from AR", {
  .prepare()
  width <- 900
  height <- NA
  aspectRatio = 3 / 2
  units <- "px"

  img <- tf("test.png")
  JPlotToPNG(img, plotWigglyLines(), width = width, height = height, aspectRatio = aspectRatio, units = units)
  expect_true(file.exists(img))
  png <- readPNG(img, native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], width)
  expect_equal(info$dim[2], width / aspectRatio)
})

test_that("Width from AR", {
  .prepare()
  height <- 600
  width <- NA
  aspectRatio <- 3 / 2
  units <- "px"

  img <- tf("test.png")
  JPlotToPNG(img, plotWigglyLines(), width = width, height = height, aspectRatio = aspectRatio, units = units)
  expect_true(file.exists(img))
  png <- readPNG(img, native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], height * aspectRatio)
  expect_equal(info$dim[2], height)
})

test_that("No Width or height fails", {
  .prepare()
  height <- NA
  width <- NA
  aspectRatio <- 3 / 2
  units <- "px"

  img <- tf("test.png")
  expect_error(JPlotToPNG(img, plotWigglyLines(), width = width, height = height, aspectRatio = aspectRatio, units = units))
})

test_that("report to file", {
  .prepare()
  f <- tf("test.png")
  JReportToFile(f, cat("Hello world!\n"))
  expect_true(file.exists(f))
  lines <- readLines(f)
  expect_equal(lines, "Hello world!")
})

#########################################################################

