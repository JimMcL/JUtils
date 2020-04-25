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
})

