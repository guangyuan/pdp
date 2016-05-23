partial: An R Package for Generating Partial Dependence Plots
================

Partial dependence functions
----------------------------

Partial dependence functions provide...

The partial package
-------------------

### Installation

For now, `partial` is only available from GitGub:

``` r
# Install development version from GitHub repo
devtools::install_github("bgreenwell/partial")
```

### Example usage

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
pd.temp.wind <- partial_2d(ozone.rf, "Temp", "Wind")
plotPartial(pd.temp.wind, 
            contour = FALSE,  # use lattice::wireframe 
            drape = TRUE,
            # shade = TRUE, 
            screen = list(z = 110, x = -60))
```

