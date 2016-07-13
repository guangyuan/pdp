context("Partial dependence functions")

# Data frames used in the following tests
#   regression: mtcars
#   classification: iris

test_that("partial works correctly", {
  mtcars.lm <- lm(mpg ~ ., data = mtcars)
  pd1 <- partial(mtcars.lm, pred.var = "wt", grid.resolution = 1, 
               super.type = "regression", training.data = mtcars)
  pd2 <- partial(mtcars.lm, pred.var = "wt", pred.grid = 1)
  expect_is(pd1, "data.frame")
  expect_is(pd2, "data.frame")
  expect_is(partial(mtcars.lm, pred.var = "wt", plot = TRUE), "trellis")
})
