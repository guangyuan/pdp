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

# Prediction functions
pred.ice <- function(object, newdata) predict(object, newdata)
pred.med <- function(object, newdata) median(predict(object, newdata))


################################################################################
# "cubist" objects: Cubist package
################################################################################

# Load required packages
library(Cubist)

# Fit a glm to the boston indians cmedv data
boston.cubist <- cubist(x = subset(boston, select = -cmedv),
                        y = boston$cmedv,
                        committees = 100)

# PDPs for lstat and rm
grid.arrange(
  partial(boston.cubist, pred.var = "lstat", plot = TRUE, rug = TRUE,
          train = boston),
  partial(boston.cubist, pred.var = "rm", plot = TRUE, rug = TRUE,
          train = boston),
  partial(boston.cubist, pred.var = c("lstat", "rm"), plot = TRUE,
          chull = TRUE, train = boston),
  ncol = 3
)

# User-specified prediction functions
grid.arrange(
  partial(boston.cubist, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE, train = boston),
  partial(boston.cubist, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE, train = boston),
  ncol = 2
)



################################################################################
# "lm" objects: stats package
################################################################################

# Fit a glm to the boston indians cmedv data
boston.lm <- lm(cmedv ~ ., data = boston)

# PDPs for lstat and rm
grid.arrange(
  partial(boston.lm, pred.var = "lstat", plot = TRUE, rug = TRUE),
  partial(boston.lm, pred.var = "rm", plot = TRUE, rug = TRUE),
  partial(boston.lm, pred.var = c("lstat", "rm"), plot = TRUE, chull = TRUE),
  ncol = 3
)

# User-specified prediction functions
grid.arrange(
  partial(boston.lm, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE),
  partial(boston.lm, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE),
  ncol = 2
)


################################################################################
# "glm" objects: stats package
################################################################################

# Fit a glm to the boston indians cmedv data
boston.glm <- glm(cmedv ~ ., data = boston, family)

# PDPs for lstat and rm
grid.arrange(
  partial(boston.glm, pred.var = "lstat", plot = TRUE, rug = TRUE),
  partial(boston.glm, pred.var = "rm", plot = TRUE, rug = TRUE),
  partial(boston.glm, pred.var = c("lstat", "rm"), plot = TRUE, chull = TRUE),
  ncol = 3
)

# User-specified prediction functions
grid.arrange(
  partial(boston.glm, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE),
  partial(boston.glm, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE),
  ncol = 2
)


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

# PDPs for lstat and rm
grid.arrange(
  partial(boston.gbm, pred.var = "lstat", plot = TRUE, rug = TRUE,
          n.trees = best.iter),
  partial(boston.gbm, pred.var = "rm", plot = TRUE, rug = TRUE),
  partial(boston.gbm, pred.var = c("lstat", "rm"), plot = TRUE, chull = TRUE),
  ncol = 3
)

# User-specified prediction functions
grid.arrange(
  partial(boston.gbm, pred.var = "lstat", pred.fun = function(object, newdata) {
            predict(object, newdata, n.trees = best.iter)
          }, plot = TRUE, rug = TRUE),
  partial(boston.gbm, pred.var = "lstat", pred.fun = function(object, newdata) {
            median(predict(object, newdata, n.trees = best.iter))
          }, plot = TRUE, rug = TRUE),
  ncol = 2
)

# Compare to gbm::plot.gbm
par(mfrow = c(1, 3))
plot(partial(boston.gbm, pred.var = "lstat", n.trees = best.iter), type = "l",
     main = "pdp::partial")
plot(partial(boston.gbm, pred.var = "lstat",
             pred.fun = function(object, newdata) {
               mean(predict(object, newdata, n.trees = best.iter))
             }), type = "l", main = "pdp::partial (pred.fun)")
plot(boston.gbm, i.var = "lstat", n.trees = best.iter,
     continuous.resolution = 51, main = "gbm::plot.gbm")


################################################################################
# "randomForest" objects: randomForest package
################################################################################

# Load required package
library(randomForest)

# Fit a random forest to the boston indians cmedv data
set.seed(101)
boston.rf <- randomForest(cmedv ~ ., data = boston)

# PDPs for lstat and rm
grid.arrange(
  partial(boston.rf, pred.var = "lstat", plot = TRUE, rug = TRUE),
  partial(boston.rf, pred.var = "rm", plot = TRUE, rug = TRUE),
  partial(boston.rf, pred.var = c("lstat", "rm"), plot = TRUE, chull = TRUE),
  ncol = 3
)

# User-specified prediction functions
grid.arrange(
  partial(boston.rf, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE),
  partial(boston.rf, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE),
  ncol = 2
)

# Compare to gbm::plot.gbm
par(mfrow = c(1, 2))
plot(partial(boston.rf, pred.var = "lstat"), type = "l", main = "pdp::partial")
partialPlot(boston.rf, pred.data = boston, x.var = "lstat",
            main = "randomForest::partialPlot")
