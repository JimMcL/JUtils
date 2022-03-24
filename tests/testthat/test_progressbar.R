context("Progressbar")

test_that("progressbar", {
  n <- 20
  expect_error(JBuildProgressBar("linux"))
  expect_error(JBuildProgressBar(""))

  out <- capture.output({
    pb <- JBuildProgressBar("text", n)
    for (i in 1:n) {
      pb()
    }
  })
  expect_length(out, 2) # Expect one line of constatntly updated progress, one completion line

})
