context("Utility functions")

# Data frames used in the following tests
#   regression: mtcars
#   classification: iris

test_that("avgLogit works correctly", {
  pm <- matrix(c(0.1, 0.3, 0.6), nrow = 1, ncol = 2, byrow = TRUE)
  expect_error(avgLogit(as.data.frame(pm)))
  expect_identical(avgLogit(pm, which.class = 1L), 
                   log(0.1) - (log(0.1) + log(0.3) + log(0.6)) / 3)
  expect_identical(avgLogit(pm, which.class = 2L), 
                   log(0.3) - (log(0.1) + log(0.3) + log(0.6)) / 3)
  expect_identical(avgLogit(pm, which.class = 3L), 
                   log(0.6) - (log(0.1) + log(0.3) + log(0.6)) / 3)
})

test_that("superType works correctly", {

  # Linear models
  mtcars.lm <- lm(mpg ~ ., data = mtcars)
  expect_identical(superType(mtcars.lm), "regression")

})
