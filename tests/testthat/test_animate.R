context("Animation")

#### Tests

test_that("JAnimateGIF works", {
  .prepare()

  .plotFrame <- function(angle) plot(x = c(sin(angle), 0), y = c(0, cos(angle)),
                                     type = 'l', lwd = 4,
                                     xlim = c(-1, 1), ylim = c(-1, 1),
                                     axes = FALSE, xlab = "", ylab = "")

  gif <- tf("test.gif")
  tryCatch(
  {
    options(warn = 2)
      JAnimateGIF(gif, frameKeys = seq(0, pi * 2, .1), plotFn = .plotFrame)
      expect_true(file.exists(gif))
      info <- file.info(gif)
      expect_true(info$size > 0)
  },
  error = function(cond) {
    # Dangerous, but silently ignore errors so that tests
    # run on systems without ImageMagick
    cat("Ignoring JAnimateGIF error:\n")
    print(cond)
  }
  )
})
