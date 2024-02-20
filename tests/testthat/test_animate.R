context("Animation")

#### Tests

test_that("JAnimateGIF works", {
  .prepare()

  .plotFrame <- function(angle, gifMethod) {
    plot(x = c(sin(angle), 0), y = c(0, cos(angle)),
         type = 'l', lwd = 4,
         xlim = c(-1, 1), ylim = c(-1, 1), asp = 1,
         axes = FALSE, xlab = "", ylab = "")
    rect(-1, -1, 1, 1, border = "brown", lwd = 2)
    t <- seq(0, 2 * pi, len = 360)
    lines(sin(t), cos(t), col = "#30f05060")
    mtext(gifMethod)
  }

  .testAnim <- function(gifMethod, ext = ".gif", loop = 0) {
    gif <- tf(paste0("test-", gifMethod, ext))
    JAnimateGIF(gif, gifMethod = gifMethod, frameKeys = seq(0, pi * 2, .1),
                plotFn = function(frame) .plotFrame(frame, gifMethod), width = 900, height = 700, loop = loop)
    expect_true(file.exists(gif))
    info <- file.info(gif)
    expect_true(info$size > 0)
  }

  magickInst <- requireNamespace("magick", quietly = TRUE)
  gifskiInst <- requireNamespace("gifski", quietly = TRUE)
  # cat(sprintf("\nAnimation tests: magick %sinstalled, gifski %sinstalled\n",
  #                 ifelse(magickInst, "", "not"),
  #                 ifelse(gifskiInst, "", "not")))

  if (magickInst) {
    .testAnim("magick-r")
  }
  if (gifskiInst) {
    .testAnim("gifski")
  }
  .testAnim("auto")

  tryCatch(
    {
      options(warn = 2)
      .testAnim("magick")
    },
    error = function(cond) {
      # Dangerous, but silently ignore errors so that tests
      # run on systems without ImageMagick
      cat("Ignoring JAnimateGIF error for gifMethod=auto:\n")
      print(cond)
    }
  )
  tryCatch(
    {
      options(warn = 2)
      .testAnim("ffmpeg", ".mp4", 1)
    },
    error = function(cond) {
      # Dangerous, but silently ignore errors so that tests
      # run on systems without FFMpeg
      cat("Ignoring JAnimateGIF error for gifMethod=ffmpeg:\n")
      print(cond)
    }
  )
})

