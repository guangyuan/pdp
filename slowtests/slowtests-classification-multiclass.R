#-------------------------------------------------------------------------------
#
# Slow tests for the pdp package
#
# Description: Testing pdp with various classification models for multiclass 
# (i.e., > 2 classes) classification.
#
# WARNING: This is simply a test file. These models are not trained to be 
# "optimal" in any sense.
#
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
# library(MASS)  # commented out to avoid NS conflicts with randomForest pkg
library(nnet)
library(party)
# library(partykit)  # commented out to avoid NS conflicts with party pkg
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

# nnet
set.seed(101)
iris.nnet <- nnet(Species ~ ., data = iris, size = 10, decay = 0.1, maxit = 500)

# nnet::multinom
set.seed(101)
iris.multinom <- multinom(Species ~ ., data = iris)

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
# Construct partial dependence plots 
################################################################################

# Function to construct a PDP for each class on the probability scale, then
# display all in one plot.
parDepPlot <- function(object, train = iris, ...) {
  pd1 <- partial(object, pred.var = "Petal.Length", grid.resolution = 10,
                 which.class = 1, prob = TRUE, train = train, progress = "text",
                 ...)
  pd2 <- partial(object, pred.var = "Petal.Length", grid.resolution = 10,
                 which.class = 2, prob = TRUE, train = train, progress = "text",
                 ...)
  pd3 <- partial(object, pred.var = "Petal.Length", grid.resolution = 10,
                 which.class = 3, prob = TRUE, train = train, progress = "text",
                 ...)
  pd <- rbind(cbind(pd1, "Class" = "1"), 
              cbind(pd2, "Class" = "2"), 
              cbind(pd3, "Class" = "3"))
  ggplot(pd, aes(x = Petal.Length, y = yhat, color = Class)) +
    geom_line() +
    xlab("Petal length") +
    ylab("Probability") +
    ggtitle(deparse(substitute(object))) +
    theme_light()
}

# Construct and store individual plots
iris.bagging.pdp <- parDepPlot(iris.bagging)
iris.boosting.pdp <- parDepPlot(iris.boosting)
iris.C5.0.pdp <- parDepPlot(iris.C5.0)
iris.svm.pdp <- parDepPlot(iris.svm)
iris.earth.pdp <- parDepPlot(iris.earth)  # can only use binomial
iris.gbm.pdp <- parDepPlot(iris.gbm, recursive = FALSE, n.trees = best.iter)
iris.ipred.bagging.pdp <- parDepPlot(iris.ipred.bagging)
iris.ksvm.pdp <- parDepPlot(iris.ksvm)
iris.lda.pdp <- parDepPlot(iris.lda)
iris.qda.pdp <- parDepPlot(iris.qda)
iris.nnet.pdp <- parDepPlot(iris.nnet)
iris.multinom <- parDepPlot(iris.multinom)
iris.ctree.pdp <- parDepPlot(iris.ctree)
iris.crf.pdp <- parDepPlot(iris.crf)
iris.partykit.ctree.pdp <- parDepPlot(iris.partykit.ctree)
iris.partykit.crf.pdp <- parDepPlot(iris.partykit.crf)
iris.rf.pdp <- parDepPlot(iris.rf)
iris.ranger.pdp <- parDepPlot(iris.ranger)
iris.rpart.pdp <- parDepPlot(iris.rpart)
iris.xgb.pdp <- parDepPlot(iris.xgb, subset(iris, select = -Species))

# Display all plots on one graph
grid.arrange(
  iris.bagging.pdp,
  iris.boosting.pdp,
  iris.C5.0.pdp,
  iris.svm.pdp,
  iris.earth.pdp,
  iris.gbm.pdp,
  iris.ipred.bagging.pdp,
  iris.ksvm.pdp,
  iris.lda.pdp,
  iris.qda.pdp,
  iris.nnet.pdp,
  iris.multinom,
  iris.ctree.pdp,
  iris.crf.pdp,
  iris.partykit.ctree.pdp,
  iris.partykit.crf.pdp,
  iris.rf.pdp,
  iris.ranger.pdp,
  iris.rpart.pdp,
  iris.xgb.pdp,
  ncol = 5
)
