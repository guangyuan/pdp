# R code for the manuscript
#
#   partial: An R Package for Creating Partial Dependence Plots
#
#        Author: Brandon M. Greenwell
# Last Modified: January 8, 2017


################################################################################
# Setup
################################################################################

# Set working directory (ignore for review)
# setwd("manuscript")

# List of packages required to run all the examples in this script
# pkgs <- c("caret",
#           "doParallel",
#           "dplyr",
#           "e1071",
#           "earth",
#           "ggplot2",
#           "party",
#           "pdp",
#           "randomForest",
#           "xgboost")
# install.packages(pkgs)

# Load required packages
library(caret)
library(doParallel)
# library(dplyr)  # use dplyr:: instead to avoid conflictions with plyr!
library(e1071)
library(earth)
library(ggplot2)
library(party)
library(pdp)
library(randomForest)
library(xgboost)


################################################################################
# Constructing PDPs in R
################################################################################

# Load the (corrected) Boston housing data
data(boston)

# Fit a random forest using default settings
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


################################################################################
# Single predictor PDPs
################################################################################

# Figure 2
pd.lstat <- partial(boston.rf, pred.var = "lstat")
pdf("pd_lstat.pdf", width = 8, height = 4)
pdp1 <- plotPartial(pd.lstat)
pdp2 <- plotPartial(pd.lstat, lwd = 2, smooth = TRUE,
                    ylab = expression(f(lstat)))
grid.arrange(pdp1, pdp2, ncol = 2)
dev.off()


################################################################################
# Multi-predictor PDPs
################################################################################

# Figure 3
rwb <- colorRampPalette(c("red", "white", "blue"))
pd.lstat.rm <- partial(boston.rf, pred.var = c("lstat", "rm"))
pdf("pd_lstat_rm.pdf", width = 12, height = 4)
p1 <- plotPartial(pd.lstat.rm)
p2 <- plotPartial(pd.lstat.rm, contour = TRUE, col.regions = rwb)
p3 <- plotPartial(pd.lstat.rm, levelplot = FALSE, zlab = "cmedv", drape = TRUE,
                  colorkey = FALSE, screen = list(z = -20, x = -60))
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()


################################################################################
# Avoiding extrapolation
################################################################################

# Figure 4
pdf("partial_extrap.pdf", width = 8, height = 4)
p1 <- partial(boston.rf, pred.var = "lstat", plot = TRUE, rug = TRUE)
p2 <- partial(boston.rf, pred.var = c("lstat", "rm"), plot = TRUE, chull = TRUE)
grid.arrange(p1, p2, ncol = 2)
dev.off()

# Figure 5
pdf("partial_manual.pdf", width = 12, height = 4)
p1 <- partial(boston.rf, "rm", plot = TRUE)
p2 <- partial(boston.rf, "rm", grid.resolution = 30, plot = TRUE)
p3 <- partial(boston.rf, "rm", pred.grid = data.frame(rm = 3:9), plot = TRUE)
grid.arrange(
  p1,  # Figure 5 (left)
  p2,  # Figure 5 (middle)
  p3,  # Figure 5 (right)
  ncol = 3
)
dev.off()


################################################################################
# Addressing computational concerns
################################################################################

# Load the LA ozone data
ozone <- read.csv(paste0("http://statweb.stanford.edu/~tibs/ElemStatLearn/",
                         "datasets/LAozone.data"), header = TRUE)

# Fit a MARS model
ozone.mars <- earth(ozone ~ ., data = ozone, degree = 3)

# Note: the following example will not work on Windows!

# # Figure 6 (Unix/Linux)
# pdf("partial_par.pdf", width = 7, height = 5)
# registerDoParallel(cores = 4)  # use 4 cores
# partial(ozone.mars, pred.var = c("wind", "temp", "dpg"), plot = TRUE,
#         chull = TRUE, parallel = TRUE)  # Figure 6
# dev.off()

# Figure 6
pdf("partial_par.pdf", width = 7, height = 5)
cl <- makeCluster(4)  # use 4 cores
registerDoParallel(cl)
partial(ozone.mars, pred.var = c("wind", "temp", "dpg"), plot = TRUE,
        chull = TRUE, parallel = TRUE, paropts = list(.packages = "earth"))  # Figure 6
lattice::trellis.focus("legend", side = "right", clipp.off = TRUE, highlight = FALSE)
grid::grid.text("ozone", 0.2, 1.05, hjust = 0.5, vjust = 1)
lattice::trellis.unfocus()
stopCluster(cl)
dev.off()


################################################################################
# Classification problems
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
                 which.class = i, progress = "text",
                 grid.resolution = 101)
  pd <- rbind(pd, cbind(tmp, Species = levels(iris$Species)[i]))
}
pdf("partial_iris_svm.pdf", width = 12, height = 4)
ggplot(pd, aes(x = Petal.Width, y = Petal.Length, z = yhat, fill = yhat)) +
  geom_tile() +
  geom_contour(color = "white", alpha = 0.5) +
  scale_fill_distiller(name = "Centered\nlogit", palette = "Spectral") +
  theme_bw() +
  facet_grid(~ Species)
dev.off()


################################################################################
# User-defined prediction functions
################################################################################

# Figure 8
probFun <- function(object, newdata) {
  pred <- predict(object, newdata, probability = TRUE)
  prob.setosa <- attr(pred, which = "probabilities")[, "setosa"]
  mean(prob.setosa)
}
pdf("partial_iris_svm_prob.pdf", width = 12, height = 4)
grid.arrange(
  partial(iris.svm, pred.var = "Petal.Width", pred.fun = probFun, plot = TRUE),
  partial(iris.svm, pred.var = "Petal.Length", pred.fun = probFun, plot = TRUE),
  partial(iris.svm, pred.var = c("Petal.Width", "Petal.Length"),
          pred.fun = probFun, plot = TRUE),
  ncol = 3
)
dev.off()

# Figure 9
pred.ice <- function(object, newdata) {
  predict(object, newdata)
}
pred.ice.quan <- function(object, newdata) {
  quantile(predict(object, newdata), probs = 1:9/10)
}
lstat.ice <- partial(boston.rf, pred.var = "lstat",
                     pred.fun = pred.ice)
lstat.ice.quan <- partial(boston.rf, pred.var = "lstat",
                          pred.fun = pred.ice.quan)
lstat.pdp <- partial(boston.rf, pred.var = "lstat")
ylim <- c(3.196822, 52.149681)
pdf("partial_boston_ice.pdf", width = 12, height = 4)
grid.arrange(
  plotPartial(lstat.ice, alpha = 0.1, ylim = ylim),
  plotPartial(lstat.ice.quan, ylim = ylim),
  plotPartial(lstat.pdp, ylim = ylim),
  ncol = 3
)
dev.off()

# Use partial to obtain ICE curves for rm
pred.ice <- function(object, newdata) predict(object, newdata)
rm.ice <- partial(boston.rf, pred.var = "rm", pred.fun = pred.ice)

# Figure 9
pdf("ice_boston.pdf", width = 7, height = 5)
plotPartial(rm.ice, rug = TRUE, train = boston, alpha = 0.2)
dev.off()

# Post-process rm.ice to obtain c-ICE curves
rm.ice <- rm.ice %>%
  dplyr::group_by(yhat.id) %>%
  dplyr::mutate(yhat.centered = yhat - first(yhat))

# ICE curves with PDP
p1 <- ggplot(rm.ice, aes(rm, yhat)) +
  geom_line(aes(group = yhat.id), alpha = 0.2) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1)

# c-ICE curves with centered PDP
p2 <- ggplot(rm.ice, aes(rm, yhat.centered)) +
  geom_line(aes(group = yhat.id), alpha = 0.2) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1)

# Figure 10
pdf("ice_cice_boston.pdf", width = 8, height = 4)
grid.arrange(p1, p2, ncol = 2)
dev.off()


################################################################################
# Using partial with the XGBoost library
################################################################################

# Tune an XGBoost model using 10-fold cross-validation
set.seed(202)
boston.xgb <- train(x = data.matrix(subset(boston, select = -cmedv)),
                    y = boston$cmedv,
                    method = "xgbTree",
                    metric = "Rsquared",
                    trControl = trainControl(method = "cv", number = 10),
                    tuneLength = 10)
plot(boston.xgb)
print(boston.xgb)
# Tuning parameter 'gamma' was held constant at a value of 0
# Tuning parameter 'min_child_weight' was
# held constant at a value of 1
# Rsquared was used to select the optimal model using  the largest value.
# The final values used for the model were:
#            nrounds = 100
#          max_depth = 5
#                eta = 0.3
#              gamma = 0
#   colsample_bytree = 0.8
#   min_child_weight = 1
#          subsample = 0.9444444

# Print the cross-validated resuts
# filter(boston.xgb$results, nrounds == 100 & max_depth == 5 & eta == 0.3 &
#        gamma == 0 & colsample_bytree == 0.8 & min_child_weight == 1 &
#        subsample == 0.94444444444444444444444)

# PDPs for lstat and rm
pdp.lstat <- partial(boston.xgb, pred.var = "lstat", plot = TRUE, rug = TRUE)
pdp.rm <- partial(boston.xgb, pred.var = "rm", plot = TRUE, rug = TRUE)
pdp.lstat.rm <- partial(boston.xgb, pred.var = c("lstat", "rm"),
                        plot = TRUE, chull = TRUE)

# Figure 11
pdf("boston_xgb.pdf", width = 12, height = 4)
grid.arrange(pdp.lstat, pdp.rm, pdp.lstat.rm, ncol = 3)
dev.off()

# Use use xgboost function directly
set.seed(203)
boston.xgb <- xgboost(data = data.matrix(subset(boston, select = -cmedv)),
                      label = boston$cmedv, objective = "reg:linear",
                      nrounds = 100, max_depth = 5, eta = 0.3, gamma = 0,
                      colsample_bytree = 0.8, min_child_weight = 1,
                      subsample = 0.9444444)

# pdf("boston_xgb.pdf", width = 12, height = 4)
X <- subset(boston, select = -cmedv)
grid.arrange(
  partial(boston.xgb, pred.var = "lstat", plot = TRUE, rug = TRUE, train = X),
  partial(boston.xgb, pred.var = "lstat", plot = TRUE, rug = TRUE, train = X),
  partial(boston.xgb, pred.var = c("lstat", "rm"), plot = TRUE, chull = TRUE,
          train = X),
  ncol = 3
)
# dev.off()
