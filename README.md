[![Build Status](https://travis-ci.org/bgreenwell/pdp.svg?branch=master)](https://travis-ci.org/bgreenwell/pdp)
[![Coverage Status](https://img.shields.io/codecov/c/github/bgreenwell/pdp.svg)](https://codecov.io/github/bgreenwell/pdp?branch=master)

pdp: An R Package for Generating Partial Dependence Plots
================

The purpose of this package is to provide a general framework for creating _partial dependence plots_ (PDPs) in R.

### Installation

For now, `pdp` is only available from GitGub using the `devtools` package:
``` r
# Install development version from GitHub repo
devtools::install_github("bgreenwell/pdp")
```

### Random forest example

``` r
# Required packages
library(pdp)
library(randomForest)

# Fit a random forest model to the airquality data
set.seed(131)  # for reproducibility
ozone.rf <- randomForest(Ozone ~ ., data = airquality, mtry = 3, 
                         importance = TRUE, na.action = na.omit)
print(ozone.rf)  # check model accuracy

# Investigate variable importance
varImpPlot(ozone.rf)

# Is there an interaction between Temp and Wind?
partial(ozone.rf, pred.var = c("Temp", "Wind"), chull = TRUE, plot = TRUE)
```
![](https://raw.githubusercontent.com/bgreenwell/pdp/master/pd_Temp_Wind.png)
