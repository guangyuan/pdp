# R code for the manuscript
#
#   partial: partial: An R Package for Creating Partial Dependence Plots
#
#        Author: Brandon M. Greenwell
# Last Modified: September 19, 2016


################################################################################
# Setup
################################################################################

# Set working directory
setwd("manuscript")

# For now, please install the development version of pdp from GitHub. The next
# release will be on CRAN soon. I am waiting out the grace period from my last
# submission---the only change in this version is the name of an argument in
# the two exported functions.
# install.packages("devtools")
# devtools::install_github("bgreenwell/pdp")


# List of packages required to run all the examples in this script
# pkgs <- c("caret",
#           "doParallel",
#           "e1071",
#           "earth",
#           "ggplot2",
#           "mlbench",
#           "party",
#           "pdp",
#           "randomForest")
# install.packages(pkgs)

# Install latest version of xgboost
# install.packages("drat", repos = "https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("data.table")  # required when installing xgboost from source!
# install.packages("xgboost", repos = "http://dmlc.ml/drat/", type = "source")

# Install the development version of pdp
# install.packages("devtools")
# devtools::install_github("bgreenwell/pdp")

# Load required packages
library(doParallel)
library(caret)
library(dplyr)
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

# Figure 2
pd.lstat <- partial(boston.rf, pred.var = "lstat")
pdf("pd_lstat.pdf", width = 8, height = 4)
pdp1 <- plotPartial(pd.lstat)
pdp2 <- plotPartial(pd.lstat, lwd = 2, smooth = TRUE,
                    ylab = expression(f(lstat)))
grid.arrange(pdp1, pdp2, ncol = 2)
dev.off()

# Fit a MARS model
boston.mars <- earth(cmedv ~ ., data = boston, degree = 3)

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
# Los Angeles ozone example
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

# Figure 6 (Windows)
pdf("partial_par.pdf", width = 7, height = 5)
cl <- makeCluster(4)  # use 4 cores
registerDoParallel(cl)
partial(ozone.mars, pred.var = c("wind", "temp", "dpg"), plot = TRUE,
        chull = TRUE, parallel = TRUE, paropts = list(.packages = "earth"))  # Figure 6
trellis.focus("legend", side = "right", clipp.off = TRUE, highlight = FALSE)
grid.text("ozone", 0.2, 1.05, hjust = 0.5, vjust = 1)
trellis.unfocus()
stopCluster(cl)
dev.off()

cl <- makeCluster(4)  # use 4 cores
registerDoParallel(cl)
p <- partial(ozone.mars, pred.var = c("wind", "temp", "dpg"), plot = FALSE,
             chull = TRUE, parallel = TRUE, paropts = list(.packages = "earth"))  # Figure 6
stopCluster(cl)
plotPartial(p)
trellis.focus("legend", side = "right", clipp.off = TRUE, highlight = FALSE)
grid.text("ozone", 0.2, 1.05, hjust = 0.5, vjust = 1)
trellis.unfocus()

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

# Use partial to obtain ICE curves for age
pred.ice <- function(object, newdata) predict(object, newdata)
age.ice <- partial(boston.rf, pred.var = "age", pred.fun = pred.ice)

# Post-process age.ice to obtain c-ICE curves
age.ice <- age.ice %>%
  group_by(yhat.id) %>%
  mutate(yhat.centered = yhat - first(yhat))

# ICE curves
p1 <- ggplot(age.ice, aes(age, yhat)) +
  geom_line(aes(group = yhat.id), alpha = 0.2) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1)

# c-ICE curves
p2 <- ggplot(age.ice, aes(age, yhat.centered)) +
  geom_line(aes(group = yhat.id), alpha = 0.2) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1)

# Figure 10
pdf("partial_boston_ice_pdp.pdf", width = 8, height = 4)
grid.arrange(p1, p2, ncol = 2)
dev.off()


################################################################################
# Using partial with the XGBoost library
################################################################################

# Optimize tuning parameters using 5-fold cross-validation
library(caret)
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
xgb.grid <- expand.grid(nrounds = c(1000, 2000, 3000),
                        max_depth = 1:4,
                        eta = c(0.001, 0.01, 0.1),
                        gamma = 0,
                        colsample_bytree = 1,
                        min_child_weight = 1,
                        subsample = c(0.5, 0.75, 1))
set.seed(202)
boston.xgb.tune <- train(x = data.matrix(subset(boston, select = -cmedv)),
                         y = boston$cmedv,
                         method = "xgbTree",
                         metric = "RMSE",
                         trControl = ctrl,
                         tuneGrid = xgb.grid)
plot(boston.xgb.tune)
print(boston.xgb.tune$bestTune)
# boston.xgb.tune$bestTune
#    nrounds max_depth  eta gamma colsample_bytree min_child_weight
# 44    2000         3 0.01     0                1                1
set.seed(102)
boston.xgb <- xgboost(data = data.matrix(subset(boston, select = -cmedv)),
                      label = boston$cmedv,
                      objective = "reg:linear",
                      nrounds = 3000,
                      max_depth = 3,
                      eta = 0.01,
                      subsample = 0.75)

# Figure 8
pdf("boston_xgb.pdf", width = 12, height = 4)
grid.arrange(
  partial(boston.xgb.tune, pred.var = "lstat", plot = T, rug = T),
  partial(boston.xgb.tune, pred.var = "rm", plot = T, rug = T),
  partial(boston.xgb.tune, pred.var = c("lstat", "rm"), plot = T, chull = T),
  ncol = 3)
dev.off()

# # Figure 8
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
