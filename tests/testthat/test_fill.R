context("Fill")


#### Tests

test_that("Fill a vector", {
  expect_equal(JFill(rep(NA, 5)), rep(NA, 5))
  expect_equal(JFill(rep(NA, 5), indices = TRUE), rep(as.numeric(NA), 5))
  x <- c(1, 2, 3, 4, 5)
  expect_equal(JFill(x), x)
  expect_equal(JFill(x, indices = TRUE), seq_len(5))
  x <- c(NA, 1, 2, 3, 4, 5)
  expect_equal(JFill(x), x)
  expect_equal(JFill(x, indices = TRUE), x + 1)
  x <- c(NA, 1, 2, NA, NA, 3, 4, 5, NA)
  xe <- c(NA, 1, 2, 2, 2, 3, 4, 5, 5)
  xi <- c(NA, 2, 3, 3, 3, 6, 7, 8, 8)
  expect_equal(JFill(x), xe)
  expect_equal(JFill(x, indices = TRUE), xi)
  x <- c(NA, "1", "2", NA, NA, "3", "4", "5", NA)
  xe <- c(NA, "1", "2", "2", "2", "3", "4", "5", "5")
  xi <- c(NA, 2, 3, 3, 3, 6, 7, 8, 8)
  expect_equal(JFill(x), xe)

  x <- c("", "Group 1", "", "", "", "Group 2", "", "")
  xe <- c(NA, "Group 1", "Group 1", "Group 1", "Group 1", "Group 2", "Group 2", "Group 2")
  xi <- c(NA, 2, 2, 2, 2, 6, 6, 6)
  expect_equal(JFill(x, x == ""), xe)
  expect_equal(JFill(x, x == "", indices = TRUE), xi)
})
