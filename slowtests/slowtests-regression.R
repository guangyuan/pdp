#-------------------------------------------------------------------------------
# Slow tests for the pdp package
#-------------------------------------------------------------------------------


################################################################################
# Setup
################################################################################

# Make sure all packges are up to date
# update.packages()

# Load required packages
library(Cubist)
library(earth)
library(e1071)
library(gbm)
library(kernlab)
library(nnet)
library(party)
library(pdp)
library(randomForest)
library(ranger)
library(rpart)
library(xgboost)

# Load data
data(boston)

# Prediction function for computing ICE curves
pred.ice <- function(object, newdata) {
  pred <- predict(object, newdata)
  if (is.data.frame(pred) || is.matrix(pred)) {
    pred <- pred[, 1L, drop = TRUE]
  }
  pred
}

# Prediction function for computing median PDP
pred.med <- function(object, newdata) {
  pred <- predict(object, newdata)
  if (is.data.frame(pred) || is.matrix(pred)) {
    pred <- pred[, 1L, drop = TRUE]
  }
  median(pred)
}


################################################################################
# Fit models
################################################################################

# Cubist::cubist
boston.cubist <- cubist(x = subset(boston, select = -cmedv),
                        y = boston$cmedv,
                        committees = 100)

# e1071::svm
boston.svm <- svm(cmedv ~ ., data = boston, type = "eps-regression")

# earth
boston.earth <- earth(cmedv ~ ., data = boston, degree = 1)

# gbm
set.seed(101)
boston.gbm <- gbm(cmedv ~ .,
                  data = boston,
                  distribution = "gaussian",
                  n.trees = 5000,
                  interaction.depth = 3,
                  shrinkage = 0.001,
                  verbose = TRUE)
best.iter <- gbm.perf(boston.gbm, method = "OOB")

# kernlab::svm
boston.ksvm <- ksvm(cmedv ~ ., data = boston, type = "eps-svr")

# nnet
boston.nnet <- nnet(cmedv ~ ., data = boston, size = 6, decay = 0.1,
                    linout = TRUE)

# party::ctree
boston.ctree <- ctree(cmedv ~ ., data = boston)

# party::cforest
set.seed(101)
boston.crf <- cforest(cmedv ~ ., data = boston)

# randomForest
set.seed(101)
boston.rf <- randomForest(cmedv ~ ., data = boston)

# ranger
set.seed(101)
boston.ranger <- ranger(cmedv ~ ., data = boston)

# rpart
boston.rpart <- rpart(cmedv ~ ., data = boston)

# stats::lm
boston.lm <- lm(cmedv ~ ., data = boston)

# stats::glm
boston.glm <- glm(cmedv ~ ., data = boston, family = gaussian)

# xgboost
set.seed(101)
boston.xgb <- xgboost(data = data.matrix(subset(boston, select = -cmedv)),
                      label = boston$cmedv, objective = "reg:linear",
                      nrounds = 100, max_depth = 5, eta = 0.3, gamma = 0,
                      colsample_bytree = 0.8, min_child_weight = 1,
                      subsample = 0.9444444)


################################################################################
# PDPs for a single predictor
################################################################################

# Store variable name(s) in case we want to change it later
x <- "lstat"

# Cubist::cubist
pdp.cubist <- partial(boston.cubist, pred.var = x, train = boston)

# e1071::svm
pdp.svm <- partial(boston.svm, pred.var = x)

# earth
pdp.earth <- partial(boston.earth, pred.var = x)

# gbm
pdp.gbm <- partial(boston.gbm, pred.var = x)

# kernlab::ksvm
pdp.ksvm <- partial(boston.ksvm, pred.var = x, train = boston)

# nnet
pdp.nnet <- partial(boston.nnet, pred.var = x)

# party::ctree
pdp.ctree <- partial(boston.ctree, pred.var = x)

# party::cforest
pdp.crf <- partial(boston.crf, pred.var = x, progress = "text")

# randomForest
pdp.rf <- partial(boston.rf, pred.var = x)

# ranger
pdp.ranger <- partial(boston.ranger, pred.var = x)

# rpart
pdp.rpart <- partial(boston.rpart, pred.var = x)

# stats::lm
pdp.lm <- partial(boston.lm, pred.var = x)

# stats::glm
pdp.glm <- partial(boston.glm, pred.var = x)

# xgboost
pdp.xgb <- partial(boston.xgb, pred.var = x,
                   train = subset(boston, select = -cmedv))

# Display PDPs in a rectangular grid
grid.arrange(
  plotPartial(pdp.cubist, main = "Cubist::cubist"),
  plotPartial(pdp.ctree, main = "party::ctree"),
  plotPartial(pdp.crf, main = "party::cforest"),
  plotPartial(pdp.earth, main = "earth"),
  plotPartial(pdp.gbm, main = "gbm"),
  plotPartial(pdp.rf, main = "randomForest"),
  plotPartial(pdp.ranger, main = "ranger"),
  plotPartial(pdp.rpart, main = "rpart"),
  plotPartial(pdp.lm, main = "stats::lm"),
  plotPartial(pdp.glm, main = "stats::glm"),
  plotPartial(pdp.svm, main = "e1071::svm"),
  plotPartial(pdp.ksvm, main = "kernlab::ksvm"),
  plotPartial(pdp.nnet, main = "nnet"),
  plotPartial(pdp.xgb, main = "xgboost"),
  ncol = 4
)


################################################################################
# PDPs for two predictors
################################################################################

# Store variable name(s) in case we want to change it later
x2 <- c("lstat", "rm")

# Resolution of plots
res <- 10

# Cubist::cubist
pdp2.cubist <- partial(boston.cubist, pred.var = x2, grid.resolution = res,
                       chull = TRUE, train = boston, progress = "text")

# e1071::svm
pdp2.svm <- partial(boston.svm, pred.var = x2, grid.resolution = res,
                    chull = TRUE, progress = "text")

# earth
pdp2.earth <- partial(boston.earth, pred.var = x2, grid.resolution = res,
                      chull = TRUE, progress = "text")

# gbm
pdp2.gbm <- partial(boston.gbm, pred.var = x2,
                    chull = TRUE, progress = "text")

# kernlab::ksvm
pdp2.ksvm <- partial(boston.ksvm, pred.var = x2, grid.resolution = res,
                     chull = TRUE, train = boston, progress = "text")

# nnet
pdp2.nnet <- partial(boston.nnet, pred.var = x2, grid.resolution = res,
                     chull = TRUE, progress = "text")

# party::ctree
pdp2.ctree <- partial(boston.ctree, pred.var = x2, grid.resolution = res,
                      chull = TRUE, progress = "text")

# party::cforest
pdp2.crf <- partial(boston.crf, pred.var = x2, grid.resolution = res,
                    chull = TRUE, progress = "text")

# randomForest
pdp2.rf <- partial(boston.rf, pred.var = x2, grid.resolution = res,
                   chull = TRUE, progress = "text")

# ranger
pdp2.ranger <- partial(boston.ranger, pred.var = x2, grid.resolution = res,
                       chull = TRUE, progress = "text")

# rpart
pdp2.rpart <- partial(boston.rpart, pred.var = x2, grid.resolution = res,
                      chull = TRUE, progress = "text")

# stats::lm
pdp2.lm <- partial(boston.lm, pred.var = x2, grid.resolution = res,
                   chull = TRUE, progress = "text")

# stats::glm
pdp2.glm <- partial(boston.glm, pred.var = x2, grid.resolution = res,
                    chull = TRUE, progress = "text")

# xgboost
pdp2.xgb <- partial(boston.xgb, pred.var = x2, grid.resolution = res,
                    chull = TRUE, train = subset(boston, select = -cmedv),
                    progress = "text")

# Display PDPs in a rectangular grid
grid.arrange(
  plotPartial(pdp2.cubist, main = "Cubist::cubist"),
  plotPartial(pdp2.ctree, main = "party::ctree"),
  plotPartial(pdp2.crf, main = "party::cforest"),
  plotPartial(pdp2.earth, main = "earth"),
  plotPartial(pdp2.gbm, main = "gbm"),
  plotPartial(pdp2.rf, main = "randomForest"),
  plotPartial(pdp2.ranger, main = "ranger"),
  plotPartial(pdp2.rpart, main = "rpart"),
  plotPartial(pdp2.lm, main = "stats::lm"),
  plotPartial(pdp2.glm, main = "stats::glm"),
  plotPartial(pdp2.svm, main = "e1071::svm"),
  plotPartial(pdp2.ksvm, main = "kernlab::ksvm"),
  plotPartial(pdp2.nnet, main = "nnet"),
  plotPartial(pdp2.xgb, main = "xgboost"),
  ncol = 4
)


################################################################################
# PDPs from user-specified prediction function
################################################################################

# Cubist
grid.arrange(
  partial(boston.cubist, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE, train = boston),
  partial(boston.cubist, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE, train = boston),
  ncol = 2
)

# e1071
grid.arrange(
  partial(boston.svm, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE),
  partial(boston.svm, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE),
  ncol = 2
)

# gbm
grid.arrange(
  partial(boston.gbm, pred.var = "lstat", pred.fun = function(object, newdata) {
    predict(object, newdata, n.trees = best.iter)
  }, plot = TRUE, rug = TRUE),
  partial(boston.gbm, pred.var = "lstat", pred.fun = function(object, newdata) {
    median(predict(object, newdata, n.trees = best.iter))
  }, plot = TRUE, rug = TRUE),
  ncol = 2
)

# kernlab
grid.arrange(
  partial(boston.ksvm, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE, train = boston),
  partial(boston.ksvm, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE, train = boston),
  ncol = 2
)

# nnet
grid.arrange(
  partial(boston.nnet, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE),
  partial(boston.nnet, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE),
  ncol = 2
)

# randomForest
grid.arrange(
  partial(boston.rf, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE),
  partial(boston.rf, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE),
  ncol = 2
)

# stats (lm)
grid.arrange(
  partial(boston.lm, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE),
  partial(boston.lm, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE),
  ncol = 2
)

# stats (glm)
grid.arrange(
  partial(boston.glm, pred.var = "lstat", pred.fun = pred.ice, plot = TRUE,
          rug = TRUE),
  partial(boston.glm, pred.var = "lstat", pred.fun = pred.med, plot = TRUE,
          rug = TRUE),
  ncol = 2
)


################################################################################
# Additional tests for gbm
################################################################################

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
# Additional tests for randomForest
################################################################################

# Compare to randomForest::partialPlot
par(mfrow = c(1, 2))
plot(partial(boston.rf, pred.var = "lstat"), type = "l", main = "pdp::partial")
partialPlot(boston.rf, pred.data = boston, x.var = "lstat",
            main = "randomForest::partialPlot")
