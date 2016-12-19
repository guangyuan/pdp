#-------------------------------------------------------------------------------
# Slow tests for the pdp package
#-------------------------------------------------------------------------------


################################################################################
# Setup
################################################################################

# Make sure all packges are up to date
# update.packages()

# Load required packages
library(pdp)

# Load data
data(boston)  # regression
data(pima)  # classification


################################################################################
# gbm package
################################################################################

# Load required package
library(gbm)


# Regression -------------------------------------------------------------------

# Fit a gbm to the Boston housing data
set.seed(101)
boston.gbm <- gbm(cmedv ~ .,
                  data = boston,
                  distribution = "gaussian",
                  n.trees = 10000,
                  cv.folds = 10,
                  interaction.depth = 3,
                  shrinkage = 0.01,
                  verbose = TRUE)
best.iter <- gbm.perf(boston.gbm, method = "cv")

# Compare plots for `lstat`
par(mfrow = c(1, 2))
plot(partial(boston.gbm, pred.var = "lstat", n.trees = best.iter), type = "l",
     main = "pdp::partial")
plot(boston.gbm, i.var = "lstat", n.trees = best.iter,
     continuous.resolution = 51, main = "gbm::plot.gbm")


# Classification ---------------------------------------------------------------

# Fit a gbm to the Pima indians diabetes data
set.seed(101)
pima.gbm <- gbm(unclass(diabetes) - 1 ~ .,
                data = pima,
                distribution = "bernoulli",
                n.trees = 5000,
                interaction.depth = 3,
                shrinkage = 0.001,
                verbose = TRUE)
best.iter <- gbm.perf(pima.gbm, method = "OOB")
summary(pima.gbm, n.trees = best.iter, las = 1)

# Compare plots for `glucose`
par(mfrow = c(1, 2))
plot(partial(pima.gbm, pred.var = "glucose", n.trees = best.iter), type = "l",
     main = "pdp::partial")
plot(pima.gbm, i.var = "glucose", n.trees = best.iter,
     continuous.resolution = 51, main = "gbm::plot.gbm")

# Compare plots for `glucose` and `age`
p <- plot(pima.gbm, i.var = c("glucose", "mass"), n.trees = best.iter,
          continuous.resolution = 51)
grid.arrange(
  partial(pima.gbm, pred.var = c("glucose", "mass"), plot = TRUE,
          chull = TRUE, progress = "text", n.trees = best.iter),
  p,
  ncol = 2
)


################################################################################
# randomForest package
################################################################################

# Load required package
library(randomForest)


# Regression -------------------------------------------------------------------

# Fit a random forest to the Boston housing data
set.seed(101)
boston.randomForest <- randomForest(cmedv ~ .,
                                    data = boston,
                                    ntree = 500,
                                    importance = FALSE)

# Partial dependence of rm and lstat on cmedv
grid.arrange(
  partial(boston.randomForest, pred.var = "rm", plot = TRUE),
  partial(boston.randomForest, pred.var = "lstat", plot = TRUE),
  partial(boston.randomForest, pred.var = c("rm", "lstat"), plot = TRUE),
  ncol = 3
)


# Classification ---------------------------------------------------------------

# Fit a random forest to the Pima indians diabetes data
set.seed(101)
pima.randomForest <- randomForest(diabetes ~ .,
                                  data = pima,
                                  na.action = na.omit,
                                  ntree = 500,
                                  importance = FALSE)

# Partial dependence of rm and lstat on cmedv
grid.arrange(
  partial(pima.randomForest, pred.var = "glucose", plot = TRUE),
  partial(pima.randomForest, pred.var = "mass", plot = TRUE),
  partial(pima.randomForest, pred.var = c("glucose", "mass"), plot = TRUE),
  ncol = 3
)
