context("Utility functions")

# Data frames used in the following tests
#   regression: mtcars
#   classification: iris

test_that("copyClasses works correctly", {
  # Incorrect classes
  set.seed(101)
  d1 <- data.frame(x1 = 1:3 * 1.0, 
                   x2 = rnorm(3), 
                   x3 = letters[1:3], 
                   x4 = as.factor(1:3),
                   x5 = as.factor(1:3),
                   x6 = c(1, 0, 1),
                   stringsAsFactors = TRUE)

  # Correct classes
  set.seed(101)
  d2 <- data.frame(x1 = 1:3, 
                   x2 = rnorm(3), 
                   x3 = letters[1:3], 
                   x4 = as.factor(1:3),
                   x5 = as.ordered(1:3),
                   x6 = c(TRUE, FALSE, TRUE),
                   stringsAsFactors = FALSE)

  # Copy classes from d2 to d1
  d3 <- copyClasses(d1, d2)
  d4 <- copyClasses(d1[1:2, ], d2)
  expect_identical(d2, d3)  
  expect_identical(sapply(d2, levels), sapply(d4, levels))
})

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

  # Bagging and boosting ------------------

  # adabag
  if (require(adabag, quietly = TRUE)) {
    set.seed(101)
    bagging.class <- bagging(Species ~ ., data = iris, mfinal = 1)
    boosting.class <- boosting(Species ~ ., data = iris, mfinal = 1)
    expect_identical(superType(bagging.class), "classification")
    expect_identical(superType(boosting.class), "classification")
  }

  # ipredbag

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
  if (require(earth, quietly = TRUE)) {
    earth.reg <- earth(mpg ~ ., data = mtcars, degree = 1, linpreds =  TRUE)
    earth.class <- earth(Species ~ ., data = iris, degree = 1, linpreds = TRUE,
                         glm = list(family = binomial))
    expect_identical(superType(earth.reg), "regression")
    expect_identical(superType(earth.class), "classification")
  }

  # GzLMs

  # Multinomial logit models

  # Random forests
  if (require(randomForest, quietly = TRUE)) {
    set.seed(101)
    rf.reg <- randomForest(mpg ~ ., data = mtcars, ntrees = 1)
    rf.class <- randomForest(Species ~ ., data = iris, ntrees = 1)
    rf.other <- randomForest( ~ ., data = mtcars)
    expect_identical(superType(rf.reg), "regression")
    expect_identical(superType(rf.class), "classification")
    expect_identical(superType(rf.other), "unsupervised")
  }

})
