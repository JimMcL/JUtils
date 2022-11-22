context("Plot raster")

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
    JPlotRaster(egJpeg, 6, 0, 3, position = "bottomright")
    JPlotRaster(egJpeg, 6, 0, height = 2, position = "topleft")
    JPlotRaster(egPng, 6, 0, 3, position = "topright")
    JPlotRaster(egPng, 6, 0, height = 2, position = "bottomleft")

    JPlotRaster(egJpeg, 14, 0, 3, position = "bottomright", offset = c(-0.1, 0.1))
    JPlotRaster(egJpeg, 14, 0, height = 2, position = "topleft", offset = c(0.1, -0.1))
    JPlotRaster(egPng, 14, 0, 3, position = "topright", offset = c(-0.1, -0.1))
    JPlotRaster(egPng, 14, 0, height = 2, position = "bottomleft", offset = c(0.1, 0.1))
  })
  expect_true(file.exists(img))
})

