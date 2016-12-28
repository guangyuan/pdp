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
data(pima)


################################################################################
# "bagging" objects: adabag package
################################################################################

# Load required package
library(adabag)

# Fit a glm to the Pima indians diabetes data
set.seed(101)  # for reproducibility
pima.bagging <- bagging(diabetes ~ ., data = pima, fmfinal = 500)

# Construct a PDP for glucose
partial(pima.bagging, pred.var = "glucose", plot = TRUE, progress = "text",
        rug = TRUE)

# Construct a PDP for glucose (probability scale)
partial(pima.bagging, pred.var = "glucose", plot = TRUE, progress = "text",
        pred.fun = function(object, newdata) {
          mean(predict(object, newdata)$prob[, 1L], na.rm = TRUE)
        })


################################################################################
# "bagging" objects: adabag package
################################################################################

# Load required package
library(adabag)

# Fit a glm to the Pima indians diabetes data
set.seed(101)  # for reproducibility
pima.boosting <- boosting(diabetes ~ ., data = pima, fmfinal = 500)

# Construct a PDP for glucose
partial(pima.boosting, pred.var = "glucose", plot = TRUE, progress = "text",
        rug = TRUE)

# Construct a PDP for glucose (probability scale)
partial(pima.boosting, pred.var = "glucose", plot = TRUE, progress = "text",
        pred.fun = function(object, newdata) {
          mean(predict(object, newdata)$prob[, 1L], na.rm = TRUE)
        })


################################################################################
# "glm" objects: stats package
################################################################################

# Fit a glm to the Pima indians diabetes data
pima.glm <- glm(diabetes ~ ., data = pima, family = binomial)

# Construct a PDP for glucose
partial(pima.glm, pred.var = "glucose", plot = TRUE)

# Construct a PDP for glucose (probability scale)
partial(pima.glm, pred.var = "glucose", plot = TRUE,
        pred.fun = function(object, newdata) {
          mean(predict(object, newdata, type = "response"), na.rm = TRUE)
        })



################################################################################
# "glm" object: gbm package
################################################################################

# Load required package
library(gbm)

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


# Try user-specified response scale
par(mfrow = c(1, 2))
plot(partial(pima.gbm, pred.var = "glucose",
             pred.fun = function(object, newdata) {
               mean(predict(object, newdata, type = "response",
                            n.trees = best.iter))
             },
             grid.resolution = 51, progress = "text"),
     type = "l", main = "pdp::partial")
plot(pima.gbm, i.var = "glucose", type = "response", n.trees = best.iter,
     continuous.resolution = 51)
title("gbm::plot.gbm")


################################################################################
# "randomForest" objects: randomForest package
################################################################################

# Load required package
library(randomForest)

# Fit a random forest to the Pima indians diabetes data
set.seed(101)
pima.randomForest <- randomForest(diabetes ~ .,
                                  data = pima,
                                  na.action = na.omit,
                                  ntree = 500,
                                  importance = FALSE)

# Construct a PDP for glucose
partial(pima.randomForest, pred.var = "glucose", plot = TRUE, progress = "text",
        rug = TRUE)

# Construct a PDP for glucose (probability scale)
partial(pima.randomForest, pred.var = "glucose", plot = TRUE, progress = "text",
        rug = TRUE, pred.fun = function(object, newdata) {
          mean(predict(object, newdata, type = "prob")[, 1L], na.rm = TRUE)
        })

# Construct ICE curves for triceps
partial(pima.randomForest, pred.var = "triceps", plot = TRUE,
        pred.fun = function(object, newdata) {
          boot::logit(predict(object, newdata, type = "prob")[, 2L])
        })


# Pima Indians example from ?ICEbox::ice ---------------------------------------

data(Pima.te, package = "MASS")  #Pima Indians diabetes classification
y = Pima.te$type
X = Pima.te
X$type = NULL

## build a RF:
pima_rf_mod = randomForest(x = X, y = y)

## Create an 'ice' object for the predictor "skin":
# For classification we plot the centered log-odds. If we pass a predict
# function that returns fitted probabilities, setting logodds = TRUE instructs
# the function to set each ice curve to the centered log-odds of the fitted
# probability.
plot(
  ice(pima_rf_mod, X = X, predictor = "skin", logodds = TRUE,
      predictfcn = function(object, newdata) {
        predict(object, newdata, type = "prob")[, 2L]
      })
)

# Do the same using partial
partial(pima_rf_mod, pred.var = "skin", plot = TRUE, train = X,
        pred.fun = function(object, newdata) {
          boot::logit(predict(object, newdata, type = "prob")[, 2L])
})
