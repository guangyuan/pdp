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

# nnet
pima.nnet <- nnet(diabetes ~ ., data = pima, size = 10, decay = 0.1, maxit = 500)

# party::ctree
pima.ctree <- ctree(diabetes ~ ., data = pima)

# party::cforest
set.seed(101)
pima.crf <- cforest(diabetes ~ ., data = pima)

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
# PDPs for a single predictor
################################################################################

# Store variable name(s) in case we want to change it later
x <- "glucose"

# adabag::bagging
pdp.bagging <- partial(pima.bagging, pred.var = x)

# adabag::boosting
pdp.boosting <- partial(pima.boosting, pred.var = x)

# C50::C5.0
pdp.C5.0 <- partial(pima.C5.0, pred.var = x)

# e1071::svm
pdp.svm <- partial(pima.svm, pred.var = x)

# earth
pdp.earth <- partial(pima.earth, pred.var = x, which.class = 2)

# gbm
pdp.gbm <- partial(pima.gbm, pred.var = x)

# ipred::bagging
pdp.ipred.bagging <- partial(pima.ipred.bagging, pred.var = x)

# kernlab::ksvm
pdp.ksvm <- partial(pima.ksvm, pred.var = x, train = pima)

# nnet
pdp.nnet <- partial(pima.nnet, pred.var = x, type = "classification",
                    which.class = 2)

# party::ctree
pdp.ctree <- partial(pima.ctree, pred.var = x)

# party::cforest
pdp.crf <- partial(pima.crf, pred.var = x, progress = "text")

# randomForest
pdp.rf <- partial(pima.rf, pred.var = x)

# ranger
pdp.ranger <- partial(pima.ranger, pred.var = x)

# rpart
pdp.rpart <- partial(pima.rpart, pred.var = x)

# stats::glm
pdp.glm <- partial(pima.glm, pred.var = x, which.class = 2)

# xgboost
pdp.xgb <- partial(pima.xgb, pred.var = x, which.class = 2,
                   train = subset(pima, select = -diabetes))

# Display PDPs
grid.arrange(
  plotPartial(pdp.bagging, main = "adabag::bagging"),
  plotPartial(pdp.boosting, main = "adabag::boosting"),
  plotPartial(pdp.C5.0, main = "C50::C5.0"),
  plotPartial(pdp.svm, main = "e1071::svm"),
  plotPartial(pdp.earth, main = "earth"),
  plotPartial(pdp.gbm, main = "gbm"),
  plotPartial(pdp.ipred.bagging, main = "ipred::bagging"),
  plotPartial(pdp.ksvm, main = "kernlab::ksvm"),
  plotPartial(pdp.nnet, main = "nnet"),
  plotPartial(pdp.ctree, main = "party::ctree"),
  plotPartial(pdp.crf, main = "party::cforest"),
  plotPartial(pdp.rf, main = "randomForest"),
  plotPartial(pdp.ranger, main = "ranger"),
  plotPartial(pdp.rpart, main = "rpart"),
  plotPartial(pdp.glm, main = "stats::glm"),
  plotPartial(pdp.xgb, main = "xgboost"),
  ncol = 4
)

# PDP on the probability scale using pred.fun argument with ranger model
partial(pima.ranger, pred.var = c("glucose", "age"), progress = "text",
        plot = TRUE, chull = TRUE, pred.fun = function(object, newdata) {
          mean(predict(object, data = newdata)$predictions[, 1])
        })
