test_that("sort works", {
  rev_letters <- letters[order(letters, decreasing = TRUE)]
  expect_equal(letters, sort_unique(rev_letters))
})

test_that("unique works", {
  rep_letters <- rep(letters, 2)
  expect_equal(letters, sort_unique(rep_letters))
})
