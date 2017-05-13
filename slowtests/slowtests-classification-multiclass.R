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

# nnet
iris.nnet <- nnet(Species ~ ., data = iris, size = 10, decay = 0.1, maxit = 500)

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

# Store variable name(s) in case we want to change it later
x <- "Petal.Length"

# adabag::bagging
pdp.bagging <- partial(iris.bagging, pred.var = x, which.class = 3)

# adabag::boosting
pdp.boosting <- partial(iris.boosting, pred.var = x, which.class = 3)

# C50::C5.0
pdp.C5.0 <- partial(iris.C5.0, pred.var = x, which.class = 3)

# e1071::svm
pdp.svm <- partial(iris.svm, pred.var = x, which.class = 3)

# earth
pdp.earth <- partial(iris.earth, pred.var = x, which.class = 3)

# gbm
pdp.gbm <- partial(iris.gbm, pred.var = x, which.class = 3, n.trees = best.iter)

# ipred::bagging
pdp.ipred.bagging <- partial(iris.ipred.bagging, pred.var = x, which.class = 3)

# kernlab::ksvm
pdp.ksvm <- partial(iris.ksvm, pred.var = x, train = iris, which.class = 3)

# MASS::lda
pdp.lda <- partial(iris.lda, pred.var = x, train = iris, which.class = 3)

# MASS::qda
pdp.qda <- partial(iris.qda, pred.var = x, train = iris, which.class = 3)

# nnet
pdp.nnet <- partial(iris.nnet, pred.var = x, type = "classification",
                    which.class = 3)

# party::ctree
pdp.ctree <- partial(iris.ctree, pred.var = x, which.class = 3)

# party::cforest
pdp.crf <- partial(iris.crf, pred.var = x, progress = "text", which.class = 3)

# partykit::cforest
pdp.partykit.ctree <- partial(iris.partykit.ctree, pred.var = x, train = iris,
                              which.class = 3)

# partykit::cforest
pdp.partykit.crf <- partial(iris.partykit.crf, pred.var = x, grid.res = 10,
                            train = iris, progress = "text", which.class = 3)

# randomForest
pdp.rf <- partial(iris.rf, pred.var = x, which.class = 3)

# ranger
pdp.ranger <- partial(iris.ranger, pred.var = x, which.class = 3)

# rpart
pdp.rpart <- partial(iris.rpart, pred.var = x, which.class = 3)

# xgboost
pdp.xgb <- partial(iris.xgb, pred.var = x, which.class = 3,
                   train = subset(iris, select = -Species))

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
  plotPartial(pdp.lda, main = "MASS::lda"),
  plotPartial(pdp.qda, main = "MASS::qda"),
  plotPartial(pdp.nnet, main = "nnet"),
  plotPartial(pdp.ctree, main = "party::ctree"),
  plotPartial(pdp.crf, main = "party::cforest"),
  plotPartial(pdp.partykit.ctree, main = "partykit::ctree"),
  plotPartial(pdp.partykit.crf, main = "partykit::cforest"),
  plotPartial(pdp.rf, main = "randomForest"),
  plotPartial(pdp.ranger, main = "ranger"),
  plotPartial(pdp.rpart, main = "rpart"),
  plotPartial(pdp.xgb, main = "xgboost"),
  ncol = 5
)

#
plot(partial(iris.xgb, pred.var = x, which.class = 1,
             train = subset(iris, select = -Species)), type = "l")
lines(partial(iris.xgb, pred.var = x, which.class = 2,
             train = subset(iris, select = -Species)), type = "l", col = 2)
lines(partial(iris.xgb, pred.var = x, which.class = 3,
             train = subset(iris, select = -Species)), type = "l", col = 3)


################################################################################
# PDPs for a single predictor (probability scale)
################################################################################

# Store variable name(s) in case we want to change it later
x <- "Petal.Length"

# adabag::bagging
pdp.bagging <- partial(iris.bagging, pred.var = x, prob = TRUE, which.class = 3)

# adabag::boosting
pdp.boosting <- partial(iris.boosting, pred.var = x, prob = TRUE,
                        which.class = 3)

# C50::C5.0
pdp.C5.0 <- partial(iris.C5.0, pred.var = x, prob = TRUE, which.class = 3)

# e1071::svm
pdp.svm <- partial(iris.svm, pred.var = x, prob = TRUE, which.class = 3)

# earth
pdp.earth <- partial(iris.earth, pred.var = x, prob = TRUE, which.class = 3)

# gbm
pdp.gbm <- partial(iris.gbm, pred.var = x, prob = TRUE, which.class = 3,
                   n.trees = best.iter)

# ipred::bagging
pdp.ipred.bagging <- partial(iris.ipred.bagging, pred.var = x, prob = TRUE,
                             which.class = 3)

# kernlab::ksvm
pdp.ksvm <- partial(iris.ksvm, pred.var = x, prob = TRUE, which.class = 3,
                    train = iris)

# nnet
pdp.nnet <- partial(iris.nnet, pred.var = x, type = "classification",
                    prob = TRUE, which.class = 3)

# party::ctree
pdp.ctree <- partial(iris.ctree, pred.var = x, prob = TRUE, which.class = 3)

# party::cforest
pdp.crf <- partial(iris.crf, pred.var = x, prob = TRUE, which.class = 3,
                   progress = "text")

# partykit::cforest
pdp.partykit.ctree <- partial(iris.partykit.ctree, pred.var = x, prob = TRUE,
                              which.class = 3, train = iris)

# partykit::cforest
pdp.partykit.crf <- partial(iris.partykit.crf, pred.var = x, grid.res = 10,
                            prob = TRUE, which.class = 3, train = iris,
                            progress = "text")

# randomForest
pdp.rf <- partial(iris.rf, pred.var = x, prob = TRUE, which.class = 3)

# ranger
pdp.ranger <- partial(iris.ranger, pred.var = x, prob = TRUE, which.class = 3)

# rpart
pdp.rpart <- partial(iris.rpart, pred.var = x, prob = TRUE, which.class = 3)

# xgboost
pdp.xgb <- partial(iris.xgb, pred.var = x, prob = TRUE, which.class = 3,
                   train = subset(iris, select = -Species))

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
  plotPartial(pdp.partykit.ctree, main = "partykit::ctree"),
  plotPartial(pdp.partykit.crf, main = "partykit::cforest"),
  plotPartial(pdp.rf, main = "randomForest"),
  plotPartial(pdp.ranger, main = "ranger"),
  plotPartial(pdp.rpart, main = "rpart"),
  plotPartial(pdp.xgb, main = "xgboost"),
  ncol = 4
)

# PDP on the probability scale using pred.fun argument with ranger model
partial(iris.ranger, pred.var = c("Petal.Length", "Petal.Width"),
        progress = "text", plot = TRUE, chull = TRUE,
        pred.fun = function(object, newdata) {
          mean(predict(object, data = newdata)$predictions[, 1])
        })
