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
data(boston)


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

# ipred::bagging
set.seed(101)
boston.ipred.bagging <- ipred::bagging(cmedv ~ ., data = boston, nbagg = 500)

# kernlab::svm
boston.ksvm <- ksvm(cmedv ~ ., data = boston, type = "eps-svr")

# # nnet
# boston.nnet <- nnet(cmedv ~ ., data = boston, size = 6, decay = 0.1,
#                     linout = TRUE)

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
                      nrounds = 100, max_depth = 3, eta = 0.01,
                      save_period = NULL)


################################################################################
# PDPs for a single predictor
################################################################################

# For brevity
parDepPlot1 <- function(object, train = boston, ...) {
  pd <- partial(object, pred.var = "lstat", grid.resolution = 10, train = train,
                progress = "text", ...)
  plotPartial(pd, main = deparse(substitute(object)))
}

# Display PDPs in a rectangular grid
grid.arrange(
  parDepPlot1(boston.cubist),
  parDepPlot1(boston.ctree),
  parDepPlot1(boston.crf),
  parDepPlot1(boston.earth),
  parDepPlot1(boston.gbm, n.trees = best.iter),
  parDepPlot1(boston.ipred.bagging),
  parDepPlot1(boston.rf),
  parDepPlot1(boston.ranger),
  parDepPlot1(boston.rpart),
  parDepPlot1(boston.lm),
  parDepPlot1(boston.glm),
  parDepPlot1(boston.svm),
  parDepPlot1(boston.ksvm),
  # parDepPlot1(boston.nnet),
  parDepPlot1(boston.xgb, train = subset(boston, select = -cmedv)),
  ncol = 4
)


################################################################################
# PDPs for two predictors
################################################################################

# For brevity
parDepPlot2 <- function(object, train = boston, ...) {
  pd <- partial(object, pred.var = c("lstat", "rm"), grid.resolution = 10,
                chull = TRUE, train = train, progress = "text", ...)
  plotPartial(pd, main = deparse(substitute(object)))
}

# Display PDPs in a rectangular grid
grid.arrange(
  parDepPlot2(boston.cubist),
  parDepPlot2(boston.ctree),
  parDepPlot2(boston.crf),
  parDepPlot2(boston.earth),
  parDepPlot2(boston.gbm, n.trees = best.iter),
  parDepPlot2(boston.rf),
  parDepPlot2(boston.ranger),
  parDepPlot2(boston.rpart),
  parDepPlot2(boston.lm),
  parDepPlot2(boston.glm),
  parDepPlot2(boston.svm),
  parDepPlot2(boston.ksvm),
  # parDepPlot2(boston.nnet),
  parDepPlot2(boston.xgb, train = subset(boston, select = -cmedv)),
  ncol = 4
)
