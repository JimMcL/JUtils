context("Plot to file")
#library(JUtils)
library(png)
library(tiff)

.mmToInches <- function(x) (x / 25.4)
.cmToInches <- function(x) (x / 2.54)

plotWigglyLines <- function(cex = 1, lwd = 2, ...) {
  set.seed(1)
  graphics::plot(rnorm(20), type = "l", main = "Wiggly Lines", ylim = c(-2.2, 3), lwd = lwd, cex = cex, cex.main = cex, ...)
  graphics::lines(rnorm(20), col = "red", lty = 2, lwd = lwd, ...)
  legend("topright", legend = c("A black line", "A dashed red line"), lty = c(1, 2), col = c("black", "red"), lwd = lwd, ...)
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

test_that("svg plotting", {
  .prepare()
  width <- 180
  height <- 120
  units <- "mm"

  img <- tf("test.svg")
  JPlotToSVG(img, plotWigglyLines(), width = width, height = height, units = units)
  expect_true(file.exists(img))
  lines <- readLines(img, n=10)
})

test_that("svg transparency", {
  .prepare()

  plotAlpha <- function() {
    graphics::plot(1:3, type = 'l', lwd = 20, col = "#ffcc8888")
    graphics::lines(3:1, lwd = 20, col = "#88ccff88")
  }
  f <- tf("test2.svg")
  expect_silent(JPlotToSVG(f, plotAlpha))
})

test_that("eps plotting", {
  .prepare()
  width <- 180
  height <- 120
  units <- "mm"

  # Note Photoshop CS2 doesn't seem to display fonts in EPS files properly

  img <- tf("test1.eps")
  JPlotToEPS(img, plotWigglyLines(), width = width, height = height, units = units)
  expect_true(file.exists(img))
  lines1 <- readLines(img, n=10)

  # Identical except for font
  img <- tf("test2.eps")
  JPlotToEPS(img, plotWigglyLines(), width = width, height = height, units = units, family = "mono")
  expect_true(file.exists(img))
  lines2 <- readLines(img, n=10)
  expect_true(lines1[2] != lines2[2])
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
  lines <- readLines(imgs[1], n=10, warn = FALSE)
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
  lines <- readLines(imgs[1], n=10, warn = FALSE)
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
  f <- tf("test.txt")
  JReportToFile(f, cat("Hello world!\n"))
  expect_true(file.exists(f))
  lines <- readLines(f)
  expect_equal(lines, "Hello world!")
})

test_that("report to NULL file", {
  .prepare()
  lines <- capture.output(JReportToFile(NULL, cat("Hello world!\n")))
  expect_equal(lines, "Hello world!")
})

test_that("test directory creation", {
  # Start with no output directory
  unlink(TEST_DIR, recursive = TRUE)
  f <- tf("test.txt")
  content <- "Hello world!\n"
  # Expect failure if not creating directory
  expect_error(JReportToFile(f, cat(content), createDirectory = FALSE))
  expect_false(file.exists(f))
  # Expect success if  creating directory
  expect_error(JReportToFile(f, cat(content), createDirectory = TRUE), NA)
  expect_true(file.exists(f))
  text <- readChar(f, 100)
  # Hack to ignore line endings
  text <- gsub("\r", "", text)
  expect_equal(text, content)
})

test_that("test postscript transparency", {
  .prepare()

  plotAlpha <- function() {
    graphics::plot(1:3, type = 'l', lwd = 20, col = "#ffcc8888")
    graphics::lines(3:1, lwd = 20, col = "#88ccff88")
  }
  # Expect normal postscript to produce a warning
  f <- tf("test1.eps")
  expect_warning(JPlotToEPS(f, plotAlpha))
  # Cairo postscript handles transparency by converting to raster
  f <- tf("test2.eps")
  expect_silent(JPlotToEPS(f, plotAlpha, cairo = TRUE, fallback_resolution = 400))
})

test_that("Incorrect plot return value", {
  .prepare()
  f <- tf("test.png")
  val <- JReportToFile(f, { cat("Hello world!\n"); 123 })
  expect_true(file.exists(f))
  lines <- readLines(f)
  expect_equal(lines, "Hello world!")
  expect_equal(val, 123, info = "JReportToFile with expression returned an incorrect value")
  val <- JReportToFile(f, function () { cat("Hello world!\n"); 123 })
  expect_equal(val, 123, info = "JReportToFile with function returned an incorrect value")

  img <- tf("test.png")
  val <- JPlotToPNG(img, { plotWigglyLines(); 123 })
  expect_true(file.exists(img))
  expect_equal(val, 123, info = "JPlotToPNG with expression returned an incorrect value")
  val <- JPlotToPNG(img, function () { plotWigglyLines(); 123 })
  expect_true(file.exists(img))
  expect_equal(val, 123, info = "JPlotToPNG with function returned an incorrect value")

  val <- JPlotToFile(img, { plotWigglyLines(); 123 })
  expect_equal(val, 123, info = "JPlotToFile with expression returned an incorrect value")
  val <- JPlotToFile(img, function () { plotWigglyLines(); 123 })
  expect_equal(val, 123, info = "JPlotToFile with function returned an incorrect value")

  val <- JPlotToFile(img, { plotWigglyLines(); NULL })
  expect_true(is.null(val), info = "JPlotToFile with expression returned an incorrect value")
  val <- JPlotToFile(img, function () { plotWigglyLines(); NULL })
  expect_true(is.null(val), info = "JPlotToFile with function returned an incorrect value")
})

test_that("raster drawing", {
  .prepare()

  egJpeg <- jpeg::readJPEG("test-img.jpg", native = TRUE)
  egPng <- png::readPNG("test-img.png", native = TRUE)

  # Too many args
  expect_error(JPlotRaster(egJpeg, 10, 0, 3, 3, position = "bottomright"), ".*width.*height")
  # Not enough args
  expect_error(JPlotRaster(egJpeg, 10, 0, position = "bottomright"), ".*width.*height")

  img <- tf("test.png")
  JPlotToPNG(img, {
    plotWigglyLines()
    JPlotRaster(egJpeg, 10, 0, 3, position = "bottomright")
    JPlotRaster(egJpeg, 10, 0, height = 2, position = "topleft")
    JPlotRaster(egPng, 10, 0, 3, position = "topright")
    JPlotRaster(egPng, 10, 0, height = 2, position = "bottomleft")
  })
  expect_true(file.exists(img))
})

#########################################################################

