# R code for "partial: partial: An R Package for Creating Partial Dependence
# Plots"
#
#        Author: Brandon M. Greenwell
# Date Modified: 06Jul2016 


################################################################################
# Setup
################################################################################

# Set working directory
# setwd("C:\\Users\\Brandon.Greenwell\\Desktop\\greenwell_partial_2016")
setwd("/home/w108bmg/Desktop/Dropbox/Projects/greenwell_partial_2016")

# Install required packages
# install.packages(c("doParallel", "e1071", "mlbench", "partial", "party", "randomForest"))

# Load required packages
# library(caret)
library(doParallel)
library(e1071)
# library(kernlab)
library(partial)
library(party)
library(randomForest)


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

# Fit a random forest using conditional inference trees
set.seed(102) # for reproducibility
boston.crf <- cforest(cmedv ~ ., data = boston)

# Fit a MARS model
boston.mars <- earth(cmedv ~ ., data = boston, degree = 3, 
                     pmethod = "exhaustive")
pd <- partial(boston.mars, c("rm", "lstat"), .progress = "text")
plotPartial(pd, convex.hull = TRUE, training.data = boston)

# Figure 3
pd.lstat.rm <- partial(boston.rf, pred.var = c("lstat", "rm"))
pdf("pd_lstat_rm.pdf", width = 12, height = 4)
pdp1 <- plotPartial(pd.lstat.rm)
pdp2 <- plotPartial(pd.lstat.rm, 
                    col.regions = colorRampPalette(c("white", "blue")))
pdp3 <- plotPartial(pd.lstat.rm, contour = FALSE, zlab = "cmedv", drape = TRUE, 
                    colorkey = FALSE, screen = list(z = -20, x = -60))
# print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
# print(p2, position = c(0.5, 0, 1, 1))
gridExtra::grid.arrange(pdp1, pdp2, pdp3, ncol = 3)
dev.off()

# Figure 4
pd.lstat <- partial(boston.rf, pred.var = "lstat")
pdf("partial_extrap.pdf", width = 12, height = 6)
pdp1 <- plotPartial(pd.lstat, rug = TRUE, training.data = boston)
pdp2 <- plotPartial(pd.lstat.rm, convex.hull = TRUE, training.data = boston)
gridExtra::grid.arrange(pdp1, pdp2, ncol = 2)
dev.off()

# Figure 5
pdf("partial_manual.pdf", width = 12, height = 4)
pdp1 <- plotPartial(partial(boston.rf, "rm"))
pdp2 <- plotPartial(partial(boston.rf, "rm", grid.resolution = 30))
pdp3 <- plotPartial(partial(boston.rf, "rm", pred.grid = data.frame(rm = 3:9)))
gridExtra::grid.arrange(pdp1, pdp2, pdp3, ncol = 3)
dev.off()

# Figure 6
registerDoParallel(cores = 4)  # use 4 cores
pd <- partial(boston.rf, pred.var = c("lstat", "rm", "ptratio"), 
              grid.resolution = 20, .parallel = TRUE)
pdf("partial_par.pdf", width = 7, height = 5)
plotPartial(pd, number = 4, overlap = 0.1)
dev.off()


################################################################################
# Edgar Anderson's iris data
################################################################################

# Train an SVM to Edgar Anderson's iris data using 5-fold classification
set.seed(101)
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
iris.svm.tune <- train(x = subset(iris, select = -Species),
                       y = iris$Species,
                       method = "svmRadial",
                       trControl = ctrl,
                       tuneLength = 100)

# Fit an SVM to Edgar Anderson's iris data
iris.svm <- svm(Species ~ ., data = iris, kernel = "radial", gamma = 0.75, 
                cost = 0.25, probability = TRUE)
iris.ksvm <- ksvm(Species ~ ., data = iris, kernel = "rbfdot", C = 0.25,
                  kpar = list(sigma = 0.75), prob.model = TRUE)

# Plot partial dependence for each class
pd <- NULL
for (i in 1:3) {
  tmp <- partial(iris.svm, pred.var = c("Petal.Width", "Petal.Length"),
                 which.class = i, .progress = "text")
  pd <- rbind(pd, cbind(tmp, Species = levels(iris$Species)[i]))
}
pdf("partial_iris_svm.pdf", width = 12, height = 4)
lattice::levelplot(y ~ Petal.Width * Petal.Length | Species, data = pd,
                   col.regions = colorRampPalette(c("red", "white", "blue")))
dev.off()
