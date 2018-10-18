# Some helper functions for managing files created during testing
TEST_DIR <- "files"

# Returns path of test file with the specified name
tf <- function(name) file.path(TEST_DIR, name)

cleanupTestFiles <- function() {
  do.call(file.remove, list(list.files(TEST_DIR, full.names = TRUE)))
}


.prepare <- function() {
  if (!dir.exists(TEST_DIR))
    dir.create(TEST_DIR)
  # Delete test files now and after completion
  cleanupTestFiles()
  # Completion is when the caller exits, not when this function
  #do.call("on.exit", list(substitute(on.exit(cleanupTestFiles())), add=TRUE), envir=parent.frame())
}
