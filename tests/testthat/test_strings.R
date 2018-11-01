context("Strings")

test_that("to sentence", {
  expect_equal(JToSentence(c("a", "b", "c")), "a, b and c")
  expect_equal(JToSentence(c("a", "b", "c", "d")), "a, b, c and d")
  expect_equal(JToSentence(c("a", "b")), "a and b")
  expect_equal(JToSentence(c("a")), "a")
  expect_equal(JToSentence(c()), NULL)
  expect_equal(JToSentence(c("a", "b", "c"), conjunction = " or "), "a, b or c")
  expect_equal(JToSentence(c("a", "b", "c"), " or "), "a or b and c")
  expect_equal(JToSentence(c("a", "b", "c"), ":", " or "), "a:b or c")
})
