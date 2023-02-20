# Some helper functions for managing files created during testing
TEST_DIR <- "files"

# Returns path of test file with the specified name
tf <- function(name) file.path(TEST_DIR, name)

cleanupTestFiles <- function() {
  do.call(file.remove, list(list.files(TEST_DIR, full.names = TRUE)))
}

cleanupTestDir <- function() {
  cleanupTestFiles()
  unlink(TEST_DIR, recursive = TRUE, force = TRUE)
}


.prepare <- function() {
  if (!dir.exists(TEST_DIR))
    dir.create(TEST_DIR)
  # Delete test files now and after completion
  cleanupTestFiles()
  # Completion is when the caller exits, not when this function
  #do.call("on.exit", list(substitute(on.exit(cleanupTestFiles())), add=TRUE), envir=parent.frame())
}

plotWigglyLines <- function(cex = 1, lwd = 2, ...) {
  set.seed(1)
  graphics::plot(rnorm(20), type = "l", main = "Wiggly Lines", ylim = c(-2.2, 3), lwd = lwd, cex = cex, cex.main = cex, ...)
  graphics::lines(rnorm(20), col = "red", lty = 2, lwd = lwd, ...)
  legend("topright", legend = c("A black line", "A dashed red line"), lty = c(1, 2), col = c("black", "red"), lwd = lwd, ...)
}

