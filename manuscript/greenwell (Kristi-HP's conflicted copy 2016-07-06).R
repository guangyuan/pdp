################################################################################
# R code for "partial: partial: An R Package for Creating Partial Dependence
# Plots"
################################################################################


################################################################################
# Setup
################################################################################

# Set working directory
setwd("/home/w108bmg/Desktop/Dropbox/Projects/greenwell_partial_2016")


################################################################################
# Code for paper
################################################################################

# Install the latest stable version from CRAN
# install.packages("partial")

# Install development version from GitHub
# devtools::install_github("bgreenwell/partial")

# Load the data
data(BostonHousing2, package = "mlbench")  # load the data
boston <- BostonHousing2[, -c(1, 2, 5)]

# Load required packages
library(partial)
library(randomForest)

# Fit a random forest
set.seed(101)  # for reproducibility
fit.rf <- randomForest(cmedv ~ ., data = boston, importance = TRUE)
print(fit.rf)  # check model results
pdf("boston_rf_vimp.pdf", width = 7, height = 5)
varImpPlot(fit.rf, main = "")  # dotchart of variable importance scores
dev.off()

# Call:
#   randomForest(formula = cmedv ~ ., data = boston, importance = TRUE)
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 5
#
# Mean of squared residuals: 9.114248
# % Var explained: 89.17

pdf("pd_lstat.pdf", width = 7, height = 5)
partial(fit.rf, pred.var = "lstat", plot = TRUE)
dev.off()

# Bivariate partial dependence
pd.lstat.rm <- partial(fit.rf, pred.var = c("lstat", "rm"))
pdf("pd_lstat_rm.pdf", width = 12, height = 6)
p1 <- plotPartial(pd.lstat.rm)
p2 <- plotPartial(pd.lstat.rm, contour = FALSE, zlab = "cmedv", drape = TRUE,
                  colorkey = TRUE, screen = list(z = -20, x = -60))
print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
print(p2, position = c(0.5, 0, 1, 1))
dev.off()

pd.lstat <- partial(fit.rf, pred.var = "lstat")
pdf("partial_extrap.pdf", width = 12, height = 6)
p1 <- plotPartial(pd.lstat, rug = TRUE, training.data = boston)
p2 <- plotPartial(pd.lstat.rm, convex.hull = TRUE, training.data = boston)
print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
print(p2, position = c(0.5, 0, 1, 1))
dev.off()

pdf("partial_manual.pdf", width = 12, height = 6)
p1 <- plotPartial(partial(fit.rf, "rm", grid.resolution = 30))
p2 <- plotPartial(partial(fit.rf, "rm", pred.grid = 3:9))
print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
print(p2, position = c(0.5, 0, 1, 1))
dev.off()

library(doParallel)
cl <- makeCluster(2)  # using snow-like functionality
registerDoParallel(cl)  # use cores = 2 for multicore functionality
pd3 <- partial(fit.rf, pred.var = c("lstat", "rm", "ptratio"),
               grid.resolution = 10, .parallel = TRUE)
stopCluster(cl)  # required when using snow-like functionality
pdf("partial_3.pdf", width = 7, height = 5)
plotPartial(pd3, number = 4, overlap = 0.1)
dev.off()


################################################################################
# Pima indians diabetes example
################################################################################

# Load required packages
library(partial)
library(randomForest)

# Load the Pima indians diabetes data
data(PimaIndiansDiabetes, package = "mlbench")
pima <- PimaIndiansDiabetes

# Train a SVM using 5-fold classification
set.seed(102)
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
pima.svm.tune <- train(x = subset(pima, select = -diabetes),
                       y = pima$diabetes,
                       method = "svmRadial",
                       trControl = ctrl,
                       tuneLength = 100)
plot(pima.svm.tune)




################################################################################
# Fisher's iris data
################################################################################


# Train a SVM to the iris data using 5-fold classification
set.seed(101)
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
iris.svm.tune <- train(x = subset(iris, select = -Species),
                       y = iris$Species,
                       method = "svmRadial",
                       trControl = ctrl,
                       tuneLength = 100)

# Fit a SVM using the tuned parameters
iris.svm <- svm(Species ~ ., data = iris, kernel = "radial", cost = 0.25,
                gamma = 0.75, probability = TRUE)
iris.ksvm <- ksvm(Species ~ ., data = iris, kernel = "rbfdot", C = 0.25,
                  kpar = list(sigma = 0.75), prob.model = TRUE)

# Plot partial dependence for each class
pd.svm <- NULL
for (i in 1:3) {
  tmp <- partial(iris.svm, pred.var = c("Petal.Width", "Petal.Length"),
                 which.class = i, training.data = iris,
                 grid.resolution = 100, super.type = "classification")
  pd.svm <- rbind(pd.svm, cbind(tmp, Species = levels(iris$Species)[i]))
}
pdf("partial_iris_svm.pdf", width = 12, height = 4)
levelplot(y ~ Petal.Width * Petal.Length | Species, data = pd.svm)
dev.off()

# Plot partial dependence for each class
pd.ksvm <- NULL
for (i in 1:3) {
  tmp <- partial(iris.ksvm, pred.var = c("Petal.Width", "Petal.Length"),
                 which.class = i, training.data = iris,
                 grid.resolution = 100)
  pd.ksvm <- rbind(pd.ksvm, cbind(tmp, Species = levels(iris$Species)[i]))
}
pdf("partial_iris_ksvm.pdf", width = 12, height = 4)
levelplot(y ~ Petal.Width * Petal.Length | Species, data = pd.ksvm)
dev.off()


################################################################################
# Neural network
################################################################################

library(nnet)

# Train a SVM to the iris data using 5-fold classification
set.seed(101)
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
iris.nnet.tune <- train(x = subset(iris, select = -Species),
                       y = iris$Species,
                       method = "nnet",
                       trControl = ctrl,
                       tuneLength = 10)

plot(iris.nnet.tune)

iris.nnet <- nnet(Species ~ ., data = iris, size = 3, decay = 0.0075)

# Plot partial dependence for each class
pd.nnet <- NULL
for (i in 1:3) {
  tmp <- partial(iris.nnet, pred.var = c("Petal.Width", "Petal.Length"),
                 which.class = i, training.data = iris,
                 grid.resolution = 100, super.type = "classification")
  pd.nnet <- rbind(pd.nnet, cbind(tmp, Species = levels(iris$Species)[i]))
}
pdf("partial_iris_nnet.pdf", width = 12, height = 4)
levelplot(y ~ Petal.Width * Petal.Length | Species, data = pd.nnet)
dev.off()


################################################################################
# Multivariate adaptive regression splines (MARS)
################################################################################

# Fit a MARS model
fit.earth <- earth(cmedv ~ ., data = boston, degree = 3, pmethod = "exhaustive")

# Print model summary
summary(fit.earth)

# Partial dependence function
earth.rm <- partial_1d(fit.earth, x.name = "rm")

# Compare partial and plotmo
pdf("plotmo_vs_partial.pdf", width = 7, height = 5)
plotmo(fit.earth, degree1 = "rm", degree2 = FALSE, do.par = FALSE,
       ylim = c(16.79417, 41.87501), main = "", xlab = "rm", ylab = "cmedv")
lines(earth.rm, lty = 2)
legend("topleft", legend = c("plotmo", "partial"),
       lty = 1:2, inset = 0.01)
dev.off()

# Partial dependence of cmedv on rm and ptratio
pdf("earth_rm_ptratio.pdf", width = 7, height = 5)
partial_2d(fit.earth, x1.name = "rm", x2.name = "ptratio", n1 = 51, plot = TRUE,
           .progress = "text")
dev.off()
