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
data(boston)


################################################################################
# "lm" objects: stats package
################################################################################

# Fit a glm to the boston indians cmedv data
boston.lm <- lm(cmedv ~ ., data = boston)

# Construct a PDP for lstat
partial(boston.lm, pred.var = "lstat", plot = TRUE)


################################################################################
# "gbm" object: gbm package
################################################################################

# Load required package
library(gbm)

# Fit a gbm to the boston indians cmedv data
set.seed(101)
boston.gbm <- gbm(cmedv ~ .,
                data = boston,
                distribution = "gaussian",
                n.trees = 5000,
                interaction.depth = 3,
                shrinkage = 0.001,
                verbose = TRUE)
best.iter <- gbm.perf(boston.gbm, method = "OOB")
summary(boston.gbm, n.trees = best.iter, las = 1)

# Compare plots for `lstat`
par(mfrow = c(1, 2))
plot(partial(boston.gbm, pred.var = "lstat", n.trees = best.iter), type = "l",
     main = "pdp::partial")
plot(boston.gbm, i.var = "lstat", n.trees = best.iter,
     continuous.resolution = 51, main = "gbm::plot.gbm")

# Compare plots for `lstat` and `age`
p <- plot(boston.gbm, i.var = c("lstat", "mass"), n.trees = best.iter,
          continuous.resolution = 51)
grid.arrange(
  partial(boston.gbm, pred.var = c("lstat", "mass"), plot = TRUE,
          chull = TRUE, progress = "text", n.trees = best.iter),
  p,
  ncol = 2
)


# Try user-specified response scale
par(mfrow = c(1, 2))
plot(partial(boston.gbm, pred.var = "lstat",
             pred.fun = function(object, newdata) {
               median(predict(object, newdata, type = "response",
                            n.trees = best.iter))
             },
             grid.resolution = 51, progress = "text"),
     type = "l", main = "pdp::partial")
plot(boston.gbm, i.var = "lstat", type = "response", n.trees = best.iter,
     continuous.resolution = 51)
title("gbm::plot.gbm")


# PDPs for lstat based on the mean (left) and median (right)
grid.arrange(
  partial(boston.gbm, pred.var = "lstat", plot = TRUE),
  partial(boston.gbm, pred.var = "lstat", plot = TRUE,
          pred.fun = function(object, newdata) {
            median(predict(object, newdata, n.trees = best.iter), na.rm = TRUE)
          }),
  ncol = 2
)

# ICE curves
partial(boston.gbm, pred.var = "lstat", plot = TRUE,
        pred.fun = function(object, newdata) {
          predict(object, newdata, n.trees = best.iter)
        })

# PDPs based on the mean and median
pdp <- partial(boston.gbm, pred.var = "lstat",
               pred.fun = function(object, newdata) {
                 c("pdp: mean" = mean(predict(object, newdata,
                                              n.trees = best.iter)),
                   "pdp: median" = median(predict(object, newdata,
                                                  n.trees = best.iter)))
               })
lattice::xyplot(yhat ~ lstat | yhat.id, data = pdp, type = "l")


################################################################################
# "randomForest" objects: randomForest package
################################################################################

# Load required package
library(randomForest)

# Fit a random forest to the boston indians cmedv data
set.seed(101)
boston.randomForest <- randomForest(cmedv ~ .,
                                  data = boston,
                                  na.action = na.omit,
                                  ntree = 500,
                                  importance = FALSE)

# PDPs for lstat based on the mean (left) and median (right)
grid.arrange(
  partial(boston.randomForest, pred.var = "lstat", plot = TRUE),
  partial(boston.randomForest, pred.var = "lstat", plot = TRUE,
          pred.fun = function(object, newdata) {
            median(predict(object, newdata), na.rm = TRUE)
          }),
  ncol = 2
)

# ICE curves
partial(boston.randomForest, pred.var = "lstat", plot = TRUE,
        pred.fun = function(object, newdata) {
          predict(object, newdata)
        })

# PDPs based on the mean and median
pdp <- partial(boston.randomForest, pred.var = "lstat",
               pred.fun = function(object, newdata) {
                 c("pdp: mean" = mean(predict(object, newdata)),
                   "pdp: median" = median(predict(object, newdata)))
               })
lattice::xyplot(yhat ~ lstat | yhat.id, data = pdp, type = "l")


# Use ggplot2 to show ICE curves and PDP
pred.ice <- function(object, newdata) predict(object, newdata)
lstat.pdp <- partial(boston.randomForest, pred.var = "lstat", plot = TRUE)
lstat.ice <- partial(boston.randomForest, pred.var = "lstat",
                     pred.fun = pred.ice)
library(ggplot2)
lstat.icecurves <- ggplot(lstat.ice.data, aes(lstat, yhat)) +
  geom_line(aes(group = yhat.id), alpha = 0.5) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 2)
grid.arrange(lstat.icecurves, lstat.pdp, ncol = 2)
