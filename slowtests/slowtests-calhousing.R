#-------------------------------------------------------------------------------
#
# Slow tests for the pdp package
#
# Description: Reproducing the California housing example from Section 14.1 of
# Chapter 10 in
#
#   Hastie, Trevor, Robert Tibshirani, and J. H. Friedman. The elements of 
#   statistical learning : data mining, inference, and prediction. New York: 
#   Springer, 2009. Print.
#
# WARNING: This is simply a test file. These models are not trained to be 
# "optimal" in any sense.
#
#-------------------------------------------------------------------------------


################################################################################
# Setup
################################################################################

# Load required packages
library(gbm)
library(pdp)

# Download function from gist
url <- "b1330460eec5acf1c81fae71902e331c"
devtools::source_gist(url, filename = "fetchCaliforniaHousingData.R")

# Fetch California housing data
cal <- fetchCaliforniaHousingData()
head(cal)


################################################################################
# Fit models
################################################################################

# Fit a GBM with the same parameters as in Hastie et al. (2009, pp 371-375)
set.seed(102)
cal.gbm <- gbm(AvgValue ~ ., data = cal,
               distribution = "laplace",
               n.trees = 2000,
               interaction.depth = 6,
               shrinkage = 0.1,
               bag.fraction = 1,
               train.fraction = 0.8,
               verbose = TRUE)
best.iter <- gbm.perf(cal.gbm, method = "test")


################################################################################
# Construct and display partial dependence plots
################################################################################

# There seems to be a fair amount of outliers in the datset so we have two
# options: 
#
#   (1) use the quantiles options with specific probabilities;
#   (2) use the trim.outliers option.
#
# Below we elect to use option (1).

# Partial dependence of AvgValue on MedInc, AveOccup, HouseAge, and AveRooms
pd.MedInc <- partial(cal.gbm, pred.var = "MedInc", quantiles = TRUE,
                     probs = 5:95/100, n.trees = best.iter)
pd.AveOccup <- partial(cal.gbm, pred.var = "AveOccup", quantiles = TRUE,
                       probs = 5:95/100, n.trees = best.iter)
pd.HouseAge <- partial(cal.gbm, pred.var = "HouseAge", quantiles = TRUE,
                       probs = 5:95/100, n.trees = best.iter)
pd.AveRooms <- partial(cal.gbm, pred.var = "AveRooms", quantiles = TRUE,
                       probs = 5:95/100, n.trees = best.iter)

# Partial dependence of AvgValue on AveOccup and HouseAge (together)
pd.HouseAge.AveOccup <- partial(
  cal.gbm, pred.var = c("HouseAge", "AveOccup"), quantiles = TRUE,
  probs = 5:95/100, n.trees = best.iter
)

# Single-predictor PDPs
ylim <- c(1, 3.5)
ylab <- "Partial dependence"
grid.arrange(
  plotPartial(pd.MedInc, rug = TRUE, train = cal, ylim = ylim, ylab = ylab),
  plotPartial(pd.AveOccup, rug = TRUE, train = cal, ylim = ylim, ylab = ylab),
  plotPartial(pd.HouseAge, rug = TRUE, train = cal, ylim = ylim, ylab = ylab),
  plotPartial(pd.AveRooms, rug = TRUE, train = cal, ylim = ylim, ylab = ylab),
  ncol = 2
)

# Two-predictor PDP
grid.arrange(
  plotPartial(pd.HouseAge.AveOccup, levelplot = FALSE, zlab = "",
              scales = list(arrows = FALSE),
              drape = TRUE, colorkey = TRUE,
              screen = list(z = 120, x = -60)),
  plotPartial(pd.HouseAge.AveOccup),
  ncol = 2
)
