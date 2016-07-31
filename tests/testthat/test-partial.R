context("Partial dependence functions")

# Data frames used in the following tests
#   regression: mtcars
#   classification: iris

test_that("partial works correctly", {

  # Regression
  mtcars.lm <- lm(mpg ~ ., data = mtcars)
  pd1 <- partial(mtcars.lm, pred.var = "wt", grid.resolution = 1,
                 super.type = "regression", train = mtcars)
  pd2 <- partial(mtcars.lm, pred.var = "wt", pred.grid = 1)
  expect_is(pd1, "data.frame")
  expect_is(pd2, "data.frame")
  expect_is(partial(mtcars.lm, pred.var = "wt", plot = TRUE), "trellis")

  # Classification
  if (require(randomForest, quietly = TRUE)) {
    iris.rf <- randomForest(Species ~ ., data = iris)
    iris.un <- randomForest( ~ ., data = iris)
    pd <- partial(iris.rf, pred.var = "Petal.Width", grid.resolution = 1)
    expect_is(pd, "data.frame")
    expect_error(partial(iris.un, pred.var = "Petal.Width", grid.resolution = 1))
  }

})
