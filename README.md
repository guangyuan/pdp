partial: An R Package for Generating Partial Dependence Plots
================

The purpose of this package is to provide a general framework for creating _partial dependence plots_ in R.

### Installation

For now, `partial` is only available on GitGub:
``` r
# Install development version from GitHub repo
devtools::install_github("bgreenwell/partial")
```

### Random forest example

``` r
# Required packages
library(partial)
library(randomForest)

# Fit a random forest model to the airquality data
set.seed(131)  # for reproducibility
ozone.rf <- randomForest(Ozone ~ ., data = airquality, mtry = 3, 
                         importance = TRUE, na.action = na.omit)
print(ozone.rf)  # check model accuracy

# Investigate variable importance
varImpPlot(ozone.rf)

# Is there an interaction between Temp and Wind?
pd.temp.wind <- partial(ozone.rf, pred.var = c("Temp", "Wind"))
plotPartial(pd.temp.wind, contour = TRUE, convex.hull = TRUE,
            training.data = airquality)
```

