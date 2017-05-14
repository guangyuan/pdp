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
library(nnet)
library(party)
# library(partykit)
library(pdp)
library(randomForest)
library(ranger)
library(rpart)
library(xgboost)

# Load data
data(pima)
pima <- na.omit(pima)


################################################################################
# Fit models
################################################################################

# adabag::bagging
set.seed(101)
pima.bagging <- bagging(diabetes ~ ., data = pima)

# adabag::boosting
set.seed(101)
pima.boosting <- boosting(diabetes ~ ., data = pima)

# C50::C5.0
set.seed(101)
pima.C5.0 <- C5.0(diabetes ~ ., data = pima, trials = 100)

# e1071::svm
pima.svm <- svm(diabetes ~ ., data = pima, type = "C-classification",
                probability = TRUE)

# earth
pima.earth <- earth(diabetes ~ ., data = pima, degree = 2,
                    glm = list(family = binomial))

# gbm
set.seed(101)
pima.gbm <- gbm(unclass(diabetes) - 1 ~ .,
                data = pima,
                distribution = "bernoulli",
                n.trees = 5000,
                interaction.depth = 3,
                shrinkage = 0.001,
                verbose = TRUE)
best.iter <- gbm.perf(pima.gbm, method = "OOB")

# ipred::bagging
set.seed(101)
pima.ipred.bagging <- ipred::bagging(diabetes ~ ., data = pima, nbagg = 500)


# kernlab::svm
pima.ksvm <- ksvm(diabetes ~ ., data = pima, type = "C-svc", prob.model = TRUE)

# # nnet
# pima.nnet <- nnet(diabetes ~ ., data = pima, size = 10, decay = 0.1, maxit = 500)

# party::ctree
pima.ctree <- ctree(diabetes ~ ., data = pima)

# party::cforest
set.seed(101)
pima.crf <- cforest(diabetes ~ ., data = pima)

# partykit::ctree
pima.partykit.ctree <- partykit::ctree(diabetes ~ ., data = pima)

# partykit::cforest
set.seed(101)
pima.partykit.crf <- partykit::cforest(diabetes ~ ., data = pima)

# randomForest
set.seed(101)
pima.rf <- randomForest(diabetes ~ ., data = pima)

# ranger
set.seed(101)
pima.ranger <- ranger(diabetes ~ ., data = pima, probability = TRUE)

# rpart
pima.rpart <- rpart(diabetes ~ ., data = pima)

# stats::glm
pima.glm <- glm(diabetes ~ ., data = pima, family = binomial)

# xgboost
set.seed(101)
pima.xgb <- xgboost(data = data.matrix(subset(pima, select = -diabetes)),
                    label = unclass(pima$diabetes) - 1,
                    objective = "binary:logistic",
                    nrounds = 100, max_depth = 3, eta = 0.1, gamma = 0,
                    colsample_bytree = 0.8, min_child_weight = 1,
                    subsample = 0.7)


################################################################################
# PDPs for a single predictor (centered logit scale)
################################################################################

# For brevity
parDepPlot1 <- function(object, train = pima, ...) {
  pd <- partial(object, pred.var = "glucose", grid.resolution = 10,
                train = train, progress = "text", ...)
  plotPartial(pd, main = deparse(substitute(object)))
}

# Display PDPs
grid.arrange(
  parDepPlot1(pima.bagging),
  parDepPlot1(pima.boosting),
  parDepPlot1(pima.C5.0),
  parDepPlot1(pima.svm),
  parDepPlot1(pima.earth, which.class = 2),
  parDepPlot1(pima.gbm, which.class = 2, n.trees = best.iter),
  parDepPlot1(pima.ipred.bagging),
  parDepPlot1(pima.ksvm),
  # parDepPlot1(pima.nnet),
  parDepPlot1(pima.ctree),
  parDepPlot1(pima.crf),
  parDepPlot1(pima.partykit.ctree),
  parDepPlot1(pima.partykit.crf),
  parDepPlot1(pima.rf),
  parDepPlot1(pima.ranger),
  parDepPlot1(pima.rpart),
  parDepPlot1(pima.glm, which.class = 2),
  parDepPlot1(pima.xgb, which.class = 2,
              train = subset(pima, select = -diabetes)),
  ncol = 4
)


################################################################################
# PDPs for a single predictor (probability scale)
################################################################################

# For brevity
parDepPlot2 <- function(object, train = pima, ...) {
  pd <- partial(object, pred.var = "glucose", grid.resolution = 10, prob = TRUE,
                train = train, progress = "text", ...)
  plotPartial(pd, main = deparse(substitute(object)))
}

# Display PDPs
grid.arrange(
  parDepPlot2(pima.bagging),
  parDepPlot2(pima.boosting),
  parDepPlot2(pima.C5.0),
  parDepPlot2(pima.svm),
  parDepPlot2(pima.earth, which.class = 2),
  parDepPlot2(pima.gbm, which.class = 2, n.trees = best.iter),
  parDepPlot2(pima.ipred.bagging),
  parDepPlot2(pima.ksvm),
  # parDepPlot2(pima.nnet),
  parDepPlot2(pima.ctree),
  parDepPlot2(pima.crf),
  parDepPlot2(pima.partykit.ctree),
  parDepPlot2(pima.partykit.crf),
  parDepPlot2(pima.rf),
  parDepPlot2(pima.ranger),
  parDepPlot2(pima.rpart),
  parDepPlot2(pima.glm, which.class = 2),
  parDepPlot2(pima.xgb, which.class = 2,
              train = subset(pima, select = -diabetes)),
  ncol = 4
)

# PDP on the probability scale using pred.fun argument with ranger model
partial(pima.ranger, pred.var = c("glucose", "age"), progress = "text",
        plot = TRUE, chull = TRUE, pred.fun = function(object, newdata) {
          mean(predict(object, data = newdata)$predictions[, 1])
        })


################################################################################
# c-ICE curves (centered logit scale)
################################################################################

# For brevity
plotIceCurves <- function(object, train = pima, ...) {
  pd <- partial(object, pred.var = "glucose", grid.resolution = 10, ice = TRUE,
                center = TRUE, train = train, progress = "text", ...)
  plotPartial(pd, main = deparse(substitute(object)), alpha = 0.25)
}

# Display PDPs
grid.arrange(
  plotIceCurves(pima.bagging),
  plotIceCurves(pima.boosting),
  plotIceCurves(pima.C5.0),
  plotIceCurves(pima.svm),
  plotIceCurves(pima.earth, which.class = 2),
  plotIceCurves(pima.gbm, which.class = 2, recursive = FALSE,
                n.trees = best.iter),
  plotIceCurves(pima.ipred.bagging),
  plotIceCurves(pima.ksvm),
  # plotIceCurves(pima.nnet),
  plotIceCurves(pima.ctree),
  plotIceCurves(pima.crf),
  plotIceCurves(pima.partykit.ctree),
  plotIceCurves(pima.partykit.crf),
  plotIceCurves(pima.rf),
  plotIceCurves(pima.ranger),
  plotIceCurves(pima.rpart),
  plotIceCurves(pima.glm, which.class = 2),
  plotIceCurves(pima.xgb, which.class = 2,
                train = subset(pima, select = -diabetes)),
  ncol = 4
)
