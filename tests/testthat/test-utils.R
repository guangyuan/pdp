context("Utility functions")

# Data frames used in the following tests
#   regression: mtcars
#   classification: iris

test_that("avgLogit works correctly", {
  pm <- matrix(c(0.1, 0.3, 0.6), nrow = 1, ncol = 3, byrow = TRUE)
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

  # Bagging ------------------
  
  # adabag
  # ipredbag
  
  # Boosting -----------------
  
  # adaboost
  # gbm
  if (require(gbm, quietly = TRUE)) {
    set.seed(101)
    gbm.reg <- gbm(mpg ~ ., data = mtcars, n.trees = 1, 
                   distribution = "gaussian", n.minobsinnode = 1)
    gbm.class <- gbm(Species ~ ., data = iris, n.trees = 1, 
                     distribution = "multinomial", n.minobsinnode = 1)
    expect_identical(superType(gbm.reg), "regression")
    expect_identical(superType(gbm.class), "classification")
  }
  
  # MARS models
  
  # GzLMs
  
  # Multinomial logit models

  # Random forests
  if (require(randomForest, quietly = TRUE)) {
    set.seed(101)
    rf.reg <- randomForest(mpg ~ ., data = mtcars, ntrees = 1)
    rf.class <- randomForest(Species ~ ., data = iris, ntrees = 1)
    expect_identical(superType(rf.reg), "regression")
    expect_identical(superType(rf.class), "classification")
  }

})
