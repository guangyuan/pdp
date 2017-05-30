context("Decision trees")

# Supported packages:
#
#   * C50
#   * party - ctree
#   * partykit - ctree
#   * rpart

test_that("C50 is supported", {

  if (require(C50, quietly = TRUE)) {

    # Fit models (C50 is for classification only)
    fit.class <- C5.0(y ~ ., data = df.class)

    # Compute partial dependence functions
    pd.class <- partial(fit.class, pred.var = "glucose", grid.resolution = 1)

    # Expectations
    expect_identical(superType(fit.class), "classification")
    expect_s3_class(pd.class, c("regression", "partial"))

  }

})


test_that("party is supported", {

  if (require(party, quietly = TRUE)) {

    # Fit models
    fit.reg <- ctree(y ~ ., data = df.reg)
    fit.class <- ctree(y ~ ., data = df.class)

    # Compute partial dependence functions
    pd.reg <- partial(fit.reg, pred.var = "lstat", grid.resolution = 1)
    pd.class <- partial(fit.class, pred.var = "glucose", grid.resolution = 1)

    # Expectations
    expect_identical(superType(fit.reg), "regression")
    expect_identical(superType(fit.class), "classification")
    expect_s3_class(pd.reg, c("regression", "partial"))
    expect_s3_class(pd.class, c("regression", "partial"))

  }

})


test_that("partykit is supported", {

  if ("partykit" %in% installed.packages()) {
  # if (require(partykit, quietly = TRUE)) {

    # Fit models
    fit.reg <- partykit::ctree(y ~ ., data = df.reg)
    fit.class <- partykit::ctree(y ~ ., data = df.class)

    # Compute partial dependence functions
    pd.reg <- partial(fit.reg, pred.var = "lstat", grid.resolution = 1)
    pd.class <- partial(fit.class, pred.var = "glucose", grid.resolution = 1)

    # Expectations
    expect_identical(superType(fit.reg), "regression")
    expect_identical(superType(fit.class), "classification")
    expect_s3_class(pd.reg, c("regression", "partial"))
    expect_s3_class(pd.class, c("regression", "partial"))

  }

})


test_that("rpart is supported", {

  if (require(rpart, quietly = TRUE)) {

    # Fit models
    fit.reg <- rpart(y ~ ., data = df.reg)
    fit.class <- rpart(y ~ ., data = df.class)

    # Compute partial dependence functions
    pd.reg <- partial(fit.reg, pred.var = "lstat", grid.resolution = 1)
    pd.class <- partial(fit.class, pred.var = "glucose", grid.resolution = 1)

    # Expectations
    expect_identical(superType(fit.reg), "regression")
    expect_identical(superType(fit.class), "classification")
    expect_s3_class(pd.reg, c("regression", "partial"))
    expect_s3_class(pd.class, c("regression", "partial"))

  }

})


context("Discriminant analysis")

# Supported packages:
#
#   * MASS: lda, qda
#   * mda: fda, mda

test_that("MASS is supported", {

  if (require(MASS, quietly = TRUE)) {

    # Fit models
    fit.lda <- lda(y ~ ., data = df.class)
    fit.qda <- qda(y ~ ., data = df.class)

    # Compute partial dependence functions
    pd.lda <- partial(fit.lda, pred.var = "glucose", grid.resolution = 1)
    pd.qda <- partial(fit.qda, pred.var = "glucose", grid.resolution = 1)

    # Expectations
    expect_identical(superType(fit.lda), "classification")
    expect_identical(superType(fit.qda), "classification")
    expect_s3_class(pd.lda, c("regression", "partial"))
    expect_s3_class(pd.qda, c("regression", "partial"))

  }

})

test_that("mda is supported", {

  if (require(mda, quietly = TRUE)) {

    # Fit models
    fit.fda <- fda(y ~ ., data = df.class)
    # fit.mda <- mda(y ~ ., data = df.class)

    # Compute partial dependence functions
    pd.fda <- partial(fit.fda, pred.var = "glucose", grid.resolution = 1)
    # pd.mda <- partial(fit.mda, pred.var = "glucose", grid.resolution = 1)

    # Expectations
    expect_identical(superType(fit.fda), "classification")
    # expect_identical(superType(fit.mda), "classification")
    expect_s3_class(pd.fda, c("regression", "partial"))
    # expect_s3_class(pd.mda, c("regression", "partial"))

  }

})
