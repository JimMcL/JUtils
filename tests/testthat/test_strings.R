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

test_that("Capitalisation", {
  expect_equal(JCapitalise(NULL), character(0))
  expect_equal(JCapitalise("the quick brown fox"), "The quick brown fox")
  expect_equal(JCapitalise("The quick brown fox"), "The quick brown fox")
  expect_equal(JCapitalise("The Quick Brown Fox"), "The Quick Brown Fox")
  expect_equal(JCapitalise("THE QUICK BROWN FOX"), "THE QUICK BROWN FOX")
  expect_equal(JCapitalise("The Quick Brown Fox", strict = TRUE), "The quick brown fox")
  expect_equal(JCapitalise("THE QUICK BROWN FOX", strict = TRUE), "The quick brown fox")
  expect_equal(JCapitalise(c("the quick brown fox", "jumped over")), c("The quick brown fox", "Jumped over"))

  expect_equal(JCapWords("the quick brown fox"), "The Quick Brown Fox")
  expect_equal(JCapWords("THE quick brown fox"), "THE Quick Brown Fox")
  expect_equal(JCapWords("THE quick brown fox", strict = TRUE), "The Quick Brown Fox")
  expect_equal(JCapWords(c("THE quick brown fox", "jumped over"), strict = TRUE), c("The Quick Brown Fox", "Jumped Over"))
})
