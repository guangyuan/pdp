context("Utility functions")

# Data frames used in the following tests
pima2 <- na.omit(pima)
set.seed(101)
df.reg <- boston[sample(nrow(boston), size = 50, replace = FALSE), ]
df.class <- pima2[sample(nrow(pima2), size = 50, replace = FALSE), ]

# Switch to generic response names
df.reg$y <- df.reg$cmedv
df.class$y <- df.class$diabetes
df.reg$cmedv <- NULL
df.class$diabetes <- NULL

test_that("copyClasses works correctly", {

  # Data frame with incorrect classes
  set.seed(101)
  d1 <- data.frame(x1 = 1:3 * 1.0,
                   x2 = rnorm(3),
                   x3 = letters[1:3],
                   x4 = as.factor(1:3),
                   x5 = as.factor(1:3),
                   x6 = c(1, 0, 1),
                   stringsAsFactors = TRUE)

  # Data frame with correct classes
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

  # Expectations
  expect_identical(d2, d3)
  expect_identical(sapply(d2, levels), sapply(d4, levels))

})

test_that("avgLogit works correctly", {

  # Probabilitymatrix/data frame
  pm <- matrix(c(0.1, 0.3, 0.6), nrow = 1, ncol = 3, byrow = TRUE)
  pm.df <- as.data.frame(pm)

  # Expectations
  expect_identical(avgLogit(pm), avgLogit(pm.df))
  expect_identical(avgLogit(pm, which.class = 1L),
                   log(0.1) - (log(0.1) + log(0.3) + log(0.6)) / 3)
  expect_identical(avgLogit(pm, which.class = 2L),
                   log(0.3) - (log(0.1) + log(0.3) + log(0.6)) / 3)
  expect_identical(avgLogit(pm, which.class = 3L),
                   log(0.6) - (log(0.1) + log(0.3) + log(0.6)) / 3)

})

test_that("trainCHull works correctly", {

  # Sample data frames
  d <- trainCHull(pred.var = c("x1", "x2"),
                  pred.grid = expand.grid(x1 = 1:10, x2 = 1:10),
                  train = expand.grid(x1 = 1:5, x2 = 1:5))

  # Expectations
  expect_is(d, "data.frame")

})


test_that("trimOutliers works correctly", {
  expect_equal(pdp:::trimOutliers(c(1:10, 1000)), 1:10)
})


test_that("predGrid works correctly", {

  # Create some toy data
  d1 <- data.frame(x1 = c(1, 5, 3), x2 = c(1, 1, 7))
  d2 <- data.frame(x1 = c(1, 5, 3), x2 = as.factor(c(1, 1, 7)))
  d3 <- data.matrix(d1)
  d4 <- data.frame(x1 = c(1, 3, 5, 1, 3, 5), x2 = c(1, 1, 1, 7, 7, 7))
  d5 <- data.frame(x1 = c(1, 3, 5, 1, 3, 5), x2 = as.factor(c(1, 1, 1, 7, 7, 7)))

  # Create grids
  d1.grid <- predGrid(d1, pred.var = c("x1", "x2"), gr = 3, cats = "x2")
  d2.grid <- predGrid(d2, pred.var = c("x1", "x2"), gr = 3)
  d3.grid <- predGrid(d3, pred.var = c("x1", "x2"), gr = 3, cats = "x2")
  d1.grid2 <- predGrid(d1, pred.var = c("x1", "x2"), cats = "x2",
                       q = TRUE, p = 0.5)
  # Expectations
  expect_identical(d1.grid, d4)
  expect_identical(d3.grid, d4)
  expect_identical(d2.grid, d5)
  expect_identical(d1.grid, d3.grid)
  expect_identical(predGrid(d1, pred.var = c("x1", "x2"), gr = 3),
                   predGrid(d3, pred.var = c("x1", "x2"), gr = 3))
  expect_identical(d1.grid2, data.frame(x1 = c(3, 3), x2 = c(1, 7)))

})

test_that("superType works correctly", {

  ##############################################################################
  # Linear models
  ##############################################################################

  # Regression
  df.reg.lm <- lm(y ~ ., data = df.reg)

  # Expectations
  expect_identical(superType(df.reg.lm), "regression")


  ##############################################################################
  # Bagging and boosting
  ##############################################################################

  # Package: adabag
  if (require(adabag, quietly = TRUE)) {

    # Classification
    set.seed(101)
    df.class.bagging <- bagging(y ~ ., data = df.class, mfinal = 1)
    df.class.boosting <- boosting(y ~ ., data = df.class, mfinal = 1)

    # Expectations
    expect_identical(superType(df.class.bagging), "classification")
    expect_identical(superType(df.class.boosting), "classification")

  }

  # Package: ipredbag

  # Package: gbm
  if (require(gbm, quietly = TRUE)) {

    # Regression
    set.seed(101)
    df.reg.gbm <- gbm(y ~ .,
                      data = df.reg,
                      n.trees = 1,
                      distribution = "gaussian",
                      n.minobsinnode = 1)

    # Classification
    set.seed(101)
    df.class.gbm <- gbm(unclass(y) - 1 ~ .,
                        data = df.class,
                        n.trees = 1,
                        distribution = "bernoulli",
                        n.minobsinnode = 1)

    # Expectations
    expect_identical(superType(df.reg.gbm), "regression")
    expect_identical(superType(df.class.gbm), "classification")

  }


  ##############################################################################
  # Multivariate adaptive regression splines
  ##############################################################################

  # Package: earth -------------------------------------------------------------
  if (require(earth, quietly = TRUE)) {

    # Regression
    df.reg.earth <- earth(y ~ .,
                          data = df.reg,
                          degree = 1,
                          linpreds =  TRUE)

    # Classification
    df.class.earth <- earth(y ~ .,
                            data = df.class,
                            degree = 1,
                            linpreds = TRUE,
                            glm = list(family = binomial))

    # Expectations
    expect_identical(superType(df.reg.earth), "regression")
    expect_identical(superType(df.class.earth), "classification")

  }


  ##############################################################################
  # Generalized linear models
  ##############################################################################

  # Package: stats -------------------------------------------------------------

  # Regression
  df.reg.glm <- glm(y ~ .,
                    data = df.reg,
                    family = gaussian)

  # Classification
  df.class.glm <- glm(y ~ .,
                      data = df.class,
                      family = binomial)

  # Package: glmnet ------------------------------------------------------------


  ##############################################################################
  # Random forests
  ##############################################################################

  # Package: randomForest ------------------------------------------------------
  if (require(randomForest, quietly = TRUE)) {

    # Regression
    set.seed(101)
    rf.reg <- randomForest(y ~ .,
                           data = df.reg,
                           ntrees = 1)

    # Classification
    set.seed(101)
    rf.class <- randomForest(y ~ .,
                             data = df.class,
                             ntrees = 1)

    # Unsupervised mode
    set.seed(101)
    rf.other <- randomForest( ~ .,
                              data = df.class)

    # Expectations
    expect_identical(superType(rf.reg), "regression")
    expect_identical(superType(rf.class), "classification")
    expect_identical(superType(rf.other), "unsupervised")

  }

})
