context("Densities plot")
#library(JUtils)



#### Tests

test_that("plotting densities", {
  .prepare()

  data <- list(
    normal = rnorm(100),
    uniform = runif(50),
    exponential = rexp(200)
  )
  densities <- lapply(data, density)
  img <- tf("densities.png")
  JPlotToFile(img, JPlotDensities(densities))
  # Just check that a file was created
  expect_true(file.exists(img))

  # Plot with fill
  JPlotToFile(img, JPlotDensities(densities, fillColours = grDevices::rainbow(3), fillAlpha = .3))
  # Just check that a file was created
  expect_true(file.exists(img))

  # Fill with lines
  JPlotToFile(img, JPlotDensities(densities, fillColours = grDevices::rainbow(3), fillAlpha = .3,
                                  fillDensities = 20, fillAngles = c(-45, 0, 45)))
  expect_true(file.exists(img))

  # Add a legend
  JPlotToFile(img, JPlotDensities(densities, fillColours = grDevices::rainbow(3), fillAlpha = .3,
                                  fillDensities = 20, fillAngles = c(-45, 0, 45),
                                  legendLabels = c("Normal", "Uniform", "Exponential")))
  expect_true(file.exists(img))

  # Add mean lines
  JPlotToFile(img, JPlotDensities(densities, fillColours = grDevices::rainbow(3), fillAlpha = .3,
                                  fillDensities = 20, fillAngles = c(-45, 0, 45),
                                  meanLty = 3,
                                  legendLabels = c("Normal", "Uniform", "Exponential")))
  expect_true(file.exists(img))
})

