context("Progressbar")

test_that("progressbar", {
  n <- 20
  expect_error(JBuildProgressBar("linux", n))
  expect_error(JBuildProgressBar("", n))

  out <- capture.output({
    pb <- JBuildProgressBar("text", n)
    for (i in 1:n) {
      pb()
      Sys.sleep(rexp(1, 100))
    }
  })
  expect_length(out, 2) # Expect one line of constantly updated progress, one completion line

  # Check showPC argument
  out <- capture.output({
    pb <- JBuildProgressBar("text", n, showPC = TRUE)
    for (i in 1:n) {
      pb()
      Sys.sleep(rexp(1, 100))
    }
  })
  expect_length(out, 2) # Expect one line of constantly updated progress, one completion line

})
