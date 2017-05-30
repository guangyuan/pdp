# context("Package support for rpart")
#
# if (require(rpart, quietly = TRUE)) {
#
#   # Fit models
#   fit.reg <- rpart(y ~ ., data = df.reg)
#   fit.class <- rpart(y ~ ., data = df.class)
#
#   # Compute partial dependence functions
#   pd.reg <- partial(fit.reg, pred.var = "lstat", grid.resolution = 1)
#   pd.class <- partial(fit.reg, pred.var = "lstat", grid.resolution = 1)
#
#   test_that("superType works correctly", {
#     expect_identical(superType(fit.reg), "regression")
#     expect_identical(superType(fit.class), "classification")
#   })
#
#
#   test_that("partial works correctly", {
#     expect_s3_class(pd.reg, c("regression", "partial"))
#     expect_s3_class(pd.class, c("regression", "partial"))
#   })
#
# }
