[![Build Status](https://travis-ci.org/bgreenwell/partial.svg?branch=master)](https://travis-ci.org/bgreenwell/partial)
[![Coverage Status](https://img.shields.io/codecov/c/github/bgreenwell/partial.svg)](https://codecov.io/github/bgreenwell/partial?branch=master)

partial: An R Package for Generating Partial Dependence Plots
================

The purpose of this package is to provide a general framework for creating _partial dependence plots_ (PDPs) in R.

### Installation

For now, `partial` is only available from GitGub using the `devtools` package:
``` r
# Install development version from GitHub repo
devtools::install_github("bgreenwell/partial")
```

### Random forest example

``` r
# Required packages
library(partial)
library(randomForest)
library(viridis)

# Fit a random forest model to the airquality data
set.seed(131)  # for reproducibility
ozone.rf <- randomForest(Ozone ~ ., data = airquality, mtry = 3, 
                         importance = TRUE, na.action = na.omit)
print(ozone.rf)  # check model accuracy

# Investigate variable importance
varImpPlot(ozone.rf)

# Is there an interaction between Temp and Wind?
pd.temp.wind <- partial(ozone.rf, pred.var = c("Temp", "Wind"), 
                        convex.hull = TRUE)
plotPartial(pd.temp.wind, contour = TRUE, col.regions = viridis)
```
![Alt text](https://raw.githubusercontent.com/bgreenwell/partial/master/pd_Temp_Wind.png)
