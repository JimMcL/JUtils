context("Plot to file")
#library(JUtils)
library(png)
library(tiff)

.mmToInches <- function(x) (x / 25.4)
.cmToInches <- function(x) (x / 2.54)

plotWigglyLines <- function(cex = 1, lwd = 2, ...) {
  set.seed(1)
  plot(rnorm(20), type = "l", main = "Wiggly Lines", ylim = c(-2.2, 3), lwd = lwd, cex = cex, cex.main = cex, ...)
  lines(rnorm(20), col = "red", lty = 2, lwd = lwd, ...)
  legend("topright", legend = c("A black line", "A dashed red line"), lty = c(1, 2), col = c("black", "red"), lwd = lwd, ...)
}

test_that("png plotting", {
  width <- 180
  height <- 120
  units <- "mm"
  res <- 72 # in ppi

  .mmToPixels <- function(x) floor(.mmToInches(x) * res)

  JPlotToPNG("test.png", plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists("test.png"))
  png <- readPNG("test.png", native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], .mmToPixels(width))
  expect_equal(info$dim[2], .mmToPixels(height))
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("png plotting in pixels", {
  width <- 800
  height <- 600
  units <- "px"
  res <- 72 # in ppi

  .mmToPixels <- function(x) floor(.mmToInches(x) * res)

  JPlotToPNG("test.png", plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists("test.png"))
  png <- readPNG("test.png", native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], width)
  expect_equal(info$dim[2], height)
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("png plotting in cm", {
  width <- 18
  height <- 12
  units <- "cm"
  res <- 100 # in ppi

  .cmToPixels <- function(x) floor(.cmToInches(x) * res)

  JPlotToPNG("test.png", plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists("test.png"))
  png <- readPNG("test.png", native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], .cmToPixels(width))
  expect_equal(info$dim[2], .cmToPixels(height))
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("png plotting in in", {
  width <- 7
  height <- 4.7
  units <- "in"
  res <- 120 # in ppi

  .inToPixels <- function(x) floor(x * res)

  JPlotToPNG("test.png", plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists("test.png"))
  png <- readPNG("test.png", native = TRUE, info = TRUE)
  info <- attr(png, "info")
  expect_equal(info$dim[1], .inToPixels(width))
  expect_equal(info$dim[2], .inToPixels(height))
  expect_equal(info$dpi[1], res, tolerance = .1)
  expect_equal(info$dpi[2], res, tolerance = .1)
})

test_that("tiff plotting", {
  width <- 180
  height <- 120
  units <- "mm"
  res <- 72 # in ppi

  .mmToPixels <- function(x) floor(.mmToInches(x) * res)

  JPlotToTIFF("test.tif", plotWigglyLines(), width = width, height = height, res = res, units = units)
  expect_true(file.exists("test.tif"))
  tiff <- readTIFF("test.tif", native = TRUE, info = TRUE)
  expect_equal(attr(tiff, "dim")[2], .mmToPixels(width))
  expect_equal(attr(tiff, "dim")[1], .mmToPixels(height))
  expect_equal(attr(tiff, "x.resolution"), res, tolerance = .1)
  expect_equal(attr(tiff, "y.resolution"), res, tolerance = .1)
})

test_that("pdf plotting", {
  width <- 180
  height <- 120
  units <- "mm"

  JPlotToPDF("test.pdf", plotWigglyLines(), width = width, height = height, units = units)
  expect_true(file.exists("test.pdf"))
  lines <- readLines("test.pdf", n=10)
})

test_that("eps plotting", {
  width <- 180
  height <- 120
  units <- "mm"

  JPlotToEPS("test.eps", plotWigglyLines(), width = width, height = height, units = units)
  expect_true(file.exists("test.eps"))
  lines <- readLines("test.eps", n=10)
})

#########################################################################

