#-------------------------------------------------------------------------------
# Slow tests for the pdp package
#-------------------------------------------------------------------------------


################################################################################
# Setup
################################################################################

# Make sure all packges are up to date
# update.packages()

# Load required packages
library(adabag)
library(C50)
library(earth)
library(e1071)
library(gbm)
library(ipred)
library(kernlab)
# library(MASS)  # for lda() and qda()
library(nnet)
library(party)
# library(partykit)
library(pdp)
library(randomForest)
library(ranger)
library(rpart)
library(xgboost)

# Load data
data(iris)


################################################################################
# Fit models
################################################################################

# adabag::bagging
set.seed(101)
iris.bagging <- bagging(Species ~ ., data = iris)

# adabag::boosting
set.seed(101)
iris.boosting <- boosting(Species ~ ., data = iris)

# C50::C5.0
set.seed(101)
iris.C5.0 <- C5.0(Species ~ ., data = iris, trials = 100)

# e1071::svm
iris.svm <- svm(Species ~ ., data = iris, type = "C-classification",
                probability = TRUE)

# earth
iris.earth <- earth(Species ~ ., data = iris, degree = 2,
                    glm = list(family = binomial))

# gbm
set.seed(101)
iris.gbm <- gbm(unclass(Species) - 1 ~ .,
                data = iris,
                distribution = "multinomial",
                n.trees = 5000,
                interaction.depth = 3,
                shrinkage = 0.001,
                verbose = TRUE)
best.iter <- gbm.perf(iris.gbm, method = "OOB")

# ipred::bagging
set.seed(101)
iris.ipred.bagging <- ipred::bagging(Species ~ ., data = iris, nbagg = 500)

# kernlab::svm
iris.ksvm <- ksvm(Species ~ ., data = iris, type = "C-svc", prob.model = TRUE)

# MASS::lda
iris.lda <- MASS::lda(Species ~ ., data = iris)

# MASS::qda
iris.qda <- MASS::qda(Species ~ ., data = iris)

# # nnet
# iris.nnet <- nnet(Species ~ ., data = iris, size = 10, decay = 0.1, maxit = 500)

# party::ctree
iris.ctree <- ctree(Species ~ ., data = iris)

# party::cforest
set.seed(101)
iris.crf <- cforest(Species ~ ., data = iris)

# partykit::ctree
iris.partykit.ctree <- partykit::ctree(Species ~ ., data = iris)

# partykit::cforest
set.seed(101)
iris.partykit.crf <- partykit::cforest(Species ~ ., data = iris)

# randomForest
set.seed(101)
iris.rf <- randomForest(Species ~ ., data = iris)

# ranger
set.seed(101)
iris.ranger <- ranger(Species ~ ., data = iris, probability = TRUE)

# rpart
iris.rpart <- rpart(Species ~ ., data = iris)

# xgboost
set.seed(101)
iris.xgb <- xgboost(data = data.matrix(subset(iris, select = -Species)),
                    label = unclass(iris$Species) - 1,
                    num_class = 3,
                    objective = "multi:softprob",
                    nrounds = 100, max_depth = 3, eta = 0.1, gamma = 0,
                    colsample_bytree = 0.8, min_child_weight = 1,
                    subsample = 0.7)


################################################################################
# PDPs for a single predictor (centered logit scale)
################################################################################

# For brevity
parDepPlot3 <- function(object, train = iris, ...) {
  pd <- partial(object, pred.var = "Petal.Length", grid.resolution = 10,
                which.class = 3, prob = TRUE, train = train, progress = "text",
                ...)
  plotPartial(pd, main = deparse(substitute(object)))
}

# Display PDPs
grid.arrange(
  parDepPlot(iris.bagging),
  parDepPlot(iris.boosting),
  parDepPlot(iris.C5.0),
  parDepPlot(iris.svm),
  parDepPlot(iris.earth),
  parDepPlot(iris.gbm, recursive = FALSE, n.trees = best.iter),
  parDepPlot(iris.ipred.bagging),
  parDepPlot(iris.ksvm),
  parDepPlot(iris.lda),
  parDepPlot(iris.qda),
  # parDepPlot(iris.nnet),
  parDepPlot(iris.ctree),
  parDepPlot(iris.crf),
  parDepPlot(iris.partykit.ctree),
  parDepPlot(iris.partykit.crf),
  parDepPlot(iris.rf),
  parDepPlot(iris.ranger),
  parDepPlot(iris.rpart),
  parDepPlot(iris.xgb, subset(iris, select = -Species)),
  ncol = 5
)

# Plot all three classes: XGBoost
plot(partial(iris.xgb, pred.var = x, which.class = 1,
             train = subset(iris, select = -Species)), type = "l")
lines(partial(iris.xgb, pred.var = x, which.class = 2,
             train = subset(iris, select = -Species)), type = "l", col = 2)
lines(partial(iris.xgb, pred.var = x, which.class = 3,
             train = subset(iris, select = -Species)), type = "l", col = 3)

# Plot all three classes: GBM (weighted tree traversal)
plot(partial(iris.gbm, pred.var = "Petal.Length", which.class = 1,
             n.trees = best.iter), type = "l")
lines(partial(iris.gbm, pred.var = "Petal.Length", which.class = 2,
              n.trees = best.iter), type = "l", col = 2)
lines(partial(iris.gbm, pred.var = "Petal.Length", which.class = 3,
              n.trees = best.iter), type = "l", col = 3)

# Plot all three classes: GBM (brute force)
plot(partial(iris.gbm, pred.var = "Petal.Length", which.class = 1,
             recursive = FALSE, n.trees = best.iter), type = "l")
lines(partial(iris.gbm, pred.var = "Petal.Length", which.class = 2,
              recursive = FALSE, n.trees = best.iter), type = "l", col = 2)
lines(partial(iris.gbm, pred.var = "Petal.Length", which.class = 3,
              recursive = FALSE, n.trees = best.iter), type = "l", col = 3)
