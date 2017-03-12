# Load required packages
library(gbm)
library(pdp)

# Download function from gist
url <- "b1330460eec5acf1c81fae71902e331c"
devtools::source_gist(url, filename = "fetchCaliforniaHousingData.R")

# Fetch California housing data
cal <- fetchCaliforniaHousingData()
head(cal)

# Run GBM
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

# PDPs
ylim <- c(1, 3.5)
ylab <- "Partial dependence"
grid.arrange(
  plotPartial(pd.MedInc, rug = TRUE, train = cal, ylim = ylim, ylab = ylab),
  plotPartial(pd.AveOccup, rug = TRUE, train = cal, ylim = ylim, ylab = ylab),
  plotPartial(pd.HouseAge, rug = TRUE, train = cal, ylim = ylim, ylab = ylab),
  plotPartial(pd.AveRooms, rug = TRUE, train = cal, ylim = ylim, ylab = ylab),
  ncol = 2
)

grid.arrange(
  plotPartial(pd.HouseAge.AveOccup, levelplot = FALSE, zlab = "",
              scales = list(arrows = FALSE),
              drape = TRUE, colorkey = TRUE,
              screen = list(z = 120, x = -60)),
  plotPartial(pd.HouseAge.AveOccup),
  ncol = 2
)
