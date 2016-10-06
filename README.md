[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pdp)](https://cran.r-project.org/package=pdp)
[![Build Status](https://travis-ci.org/bgreenwell/pdp.svg?branch=master)](https://travis-ci.org/bgreenwell/pdp)
[![Coverage Status](https://img.shields.io/codecov/c/github/bgreenwell/pdp.svg)](https://codecov.io/github/bgreenwell/pdp?branch=master)
[![Downloads](http://cranlogs.r-pkg.org/badges/pdp)](http://cranlogs.r-pkg.org/badges/pdp)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/pdp)](http://cranlogs.r-pkg.org/badges/grand-total/pdp)

pdp: An R Package for Constructing Partial Dependence Plots
================

The primary purpose of this package is to provide a general framework for constructing _partial dependence plots_ (PDPs) in R.

### Installation

The R package `pdp` is available from [CRAN](http://cran.r-project.org/package=pdp); the development version is hosted on [GitHub](https://github.com/bgreenwell/pdp). There are two ways to install:
``` r
# Install latest release from CRAN
install.packages("pdp")

# Install development version from GitHub repo
devtools::install_github("bgreenwell/pdp")
```

### Random forest example

``` r
# Install additional packages
install.packages(c("ggmap", "mlbench", "randomForest"))

# Load required packages
library(ggmap)
library(pdp)
library(randomForest)

# Load the (corrected) Boston housing data
data(BostonHousing2, package = "mlbench")
boston <- BostonHousing2[, -c(1, 2, 5)]  # remove unused columns

# Fit a random forest using default settings
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ ., data = boston, importance = TRUE)
print(boston.rf)  # check model accuracy
varImpPlot(boston.rf)  # check variable importance

# Is there an interaction between lstat and rm?
partial(boston.rf, pred.var = c("lstat", "rm"), chull = TRUE, plot = TRUE,
        .progress = "text")
        
# Look at partial dependence of median home value on location
pd.loc <- partial(boston.rf, pred.var = c("lon", "lat"), chull = TRUE,
                  .progress = "text")

# Overlay predictions on a map of Boston
ll <- c(range(boston$lon), range(boston$lat))
map <- get_map(location = ll[c(1, 3, 2, 4)], zoom = 11, maptype = "toner-lite")
ggmap(map) + 
  geom_point(aes(x = lon, y = lat), data = boston, alpha = 0.2) +
  geom_tile(aes(x = lon, y = lat, z = y, fill = y), data = pd.loc, alpha = 0.3) +
  geom_contour(color = "white", alpha = 0.5) +
  scale_fill_distiller(palette = "Spectral", name = "Median\nvalue") +
  coord_fixed(ratio = 1) +
  labs(x = "Longitude", y = "Latitude")
```
![](https://raw.githubusercontent.com/bgreenwell/pdp/master/pd_lstat_rm.png)
![](https://raw.githubusercontent.com/bgreenwell/pdp/master/pd_lon_lat.png)

