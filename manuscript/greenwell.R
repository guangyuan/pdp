# R code for the manuscript
#
#   partial: partial: An R Package for Creating Partial Dependence Plots
#
#        Author: Brandon M. Greenwell
# Date Modified: 11Sep2016


################################################################################
# Setup
################################################################################

# Set working directory
setwd("manuscript")

# List of packages required to run all the examples in this script
pkgs <- c("doParallel",
          "e1071",
          "ggplot2",
          "mlbench",
          "party",
          "pdp",
          "randomForest",
          "xgboost")
# install.packages(pkgs)

# Load required packages
library(doParallel)
library(e1071)
library(earth)
library(ggplot2)
library(party)
library(pdp)
library(randomForest)
library(xgboost)


################################################################################
# Boston housing example
################################################################################

# Load the (corrected) Boston housing data
data(BostonHousing2, package = "mlbench")
boston <- BostonHousing2[, -c(1, 2, 5)]  # remove unused columns

# Fit a random forest and a conditional random forest using default settings
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ ., data = boston, importance = TRUE)
print(boston.rf)  # check model results
# Call:
#   randomForest(formula = cmedv ~ ., data = boston, importance = TRUE)
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 5
#
# Mean of squared residuals: 9.114248
# % Var explained: 89.17

# Figure 1
pdf("boston_rf_vimp.pdf", width = 7, height = 5)
varImpPlot(boston.rf)  # dotchart of variable importance scores
dev.off()

# Figure 2
pd.lstat <- partial(boston.rf, pred.var = "lstat")
pdf("pd_lstat.pdf", width = 8, height = 4)
pdp1 <- plotPartial(pd.lstat)
pdp2 <- plotPartial(pd.lstat, lwd = 2, smooth = TRUE, span = 0.5,
                    ylab = expression(f(lstat)))
gridExtra::grid.arrange(pdp1, pdp2, ncol = 2)
dev.off()

# Fit a MARS model
boston.mars <- earth(cmedv ~ ., data = boston, degree = 3)

# Figure 3
pd.lstat.rm <- partial(boston.mars, pred.var = c("lstat", "rm"))
pdf("pd_lstat_rm.pdf", width = 12, height = 4)
pdp1 <- plotPartial(pd.lstat.rm)
pdp2 <- plotPartial(pd.lstat.rm,
                    col.regions = colorRampPalette(c("red", "white", "blue")))
pdp3 <- plotPartial(pd.lstat.rm, contour = FALSE, zlab = "cmedv", drape = TRUE,
                    colorkey = FALSE, screen = list(z = -20, x = -60))
# print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
# print(p2, position = c(0.5, 0, 1, 1))
gridExtra::grid.arrange(pdp1, pdp2, pdp3, ncol = 3)
dev.off()

# Figure 4
pd.lstat <- partial(boston.rf, pred.var = "lstat")
# pdf("partial_extrap.pdf", width = 12, height = 4)
pdf("partial_extrap.pdf", width = 8, height = 4)
pdp1 <- plotPartial(pd.lstat, rug = TRUE, train = boston)
pdp2 <- plotPartial(pd.lstat.rm, chull = TRUE, train = boston)
pdp3 <- plotPartial(partial(boston.rf, pred.var = c("lstat", "rm"),
                            chull = TRUE))
gridExtra::grid.arrange(pdp1, pdp3, ncol = 2)
dev.off()

# Figure 5
pdf("partial_manual.pdf", width = 12, height = 4)
pdp1 <- plotPartial(partial(boston.rf, "rm"))
pdp2 <- plotPartial(partial(boston.rf, "rm", grid.resolution = 30))
pdp3 <- plotPartial(partial(boston.rf, "rm", pred.grid = data.frame(rm = 3:9)))
gridExtra::grid.arrange(pdp1, pdp2, pdp3, ncol = 3)
dev.off()


################################################################################
# Los Angeles ozone example
################################################################################

# Load the LA ozone data
ozone <- read.csv(paste0("http://statweb.stanford.edu/~tibs/ElemStatLearn/",
                         "datasets/LAozone.data"), header = TRUE)

# Fit a MARS model
ozone.earth <- earth(ozone ~ ., data = ozone, degree = 3)

# Note: the following example will not work on Windows!

# Figure 6
registerDoParallel(cores = 4)  # use 4 cores
pd <- partial(ozone.earth, pred.var = c("wind", "temp", "dpg"), chull = TRUE,
              .parallel = TRUE)
pdf("partial_par.pdf", width = 7, height = 5)
plotPartial(pd)
dev.off()


################################################################################
# Edgar Anderson's iris data
################################################################################

# # Train an SVM to Edgar Anderson's iris data using 5-fold classification
# library(caret)
# set.seed(101)
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
# iris.svm.tune <- train(x = subset(iris, select = -Species),
#                        y = iris$Species,
#                        method = "svmRadial",
#                        trControl = ctrl,
#                        tuneLength = 100)

# Fit an SVM to Edgar Anderson's iris data
iris.svm <- svm(Species ~ ., data = iris, kernel = "radial", gamma = 0.75,
                cost = 0.25, probability = TRUE)

# Figure 7
pd <- NULL
for (i in 1:3) {
  tmp <- partial(iris.svm, pred.var = c("Petal.Width", "Petal.Length"),
                 which.class = i, .progress = "text",
                 grid.resolution = 101)
  pd <- rbind(pd, cbind(tmp, Species = levels(iris$Species)[i]))
}
pdf("partial_iris_svm.pdf", width = 12, height = 4)
ggplot(pd, aes(x = Petal.Width, y = Petal.Length, z = y, fill = y)) +
  geom_tile() +
  geom_contour(color = "white", alpha = 0.5) +
  scale_fill_distiller(palette = "Spectral") +
  theme_bw() +
  facet_grid(~ Species)
dev.off()


################################################################################
# Using partial with the XGBoost library
################################################################################

# Optimize tuning parameters using 5-fold cross-validation
# library(caret)
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
# xgb.grid <- expand.grid(nrounds = c(100, 500, 1000, 2000, 5000),
#                         max_depth = 1:6,
#                         eta = c(0.001, 0.01, 0.1, 0.5, 1),
#                         gamma = 0,
#                         colsample_bytree = 1,
#                         min_child_weight = 1)
# set.seed(202)
# boston.xgb.tune <- train(x = data.matrix(subset(boston, select = -cmedv)),
#                          y = boston$cmedv,
#                          method = "xgbTree",
#                          metric = "RMSE",
#                          trControl = ctrl,
#                          tuneGrid = xgb.grid)
# plot(boston.xgb.tune)
# boston.xgb.tune$bestTune
#    nrounds max_depth  eta gamma colsample_bytree min_child_weight
# 44    2000         3 0.01     0                1                1
set.seed(102)
boston.xgb <- xgboost(data = data.matrix(subset(boston, select = -cmedv)),
                      label = boston$cmedv,
                      objective = "reg:linear",
                      nrounds = 2000,
                      max_depth = 3,
                      eta = 0.01)

# partial(boston.xgb, pred.var = c("lon", "lat"), plot = TRUE, chull = TRUE,
#         train = X, .progress = "text")

# Figure 8
pdf("boston_xgb.pdf", width = 12, height = 4)
X <- subset(boston, select = -cmedv)
pdp1 <- plotPartial(partial(boston.xgb, pred.var = "lstat", train = X),
                    rug = TRUE, smooth = TRUE, train = X)
pdp2 <- plotPartial(partial(boston.xgb, pred.var = "rm", train = X),
                    rug = TRUE, smooth = TRUE, train = X)
pdp3 <- plotPartial(partial(boston.xgb, pred.var = c("lstat", "rm"),
                            chull = TRUE, train = X), rug = TRUE, train = X)
gridExtra::grid.arrange(pdp1, pdp2, pdp3, ncol = 3)
dev.off()
