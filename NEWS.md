# NEWS for pdp package

### Changes for version 0.2.0
* `randomForest` is no longer imported.
* Added support for the `caret` package (i.e., object of class `"train"`).
* Added example datasets: `boston` (corrected Boston housing data) and `pima` (corrected Pima Indians diabetes data).
* Fixed error that sometimes occurred when `chull = TRUE` causing the convex hull to not be computed.
* Refactored `plotPartial` to be more modular.
* Added `gbm` support for most non-`"binomial"` families`.

### Changes for version 0.1.0
* `randomForest` is now imported.
* Added examples.

### Changes for version 0.0.6
* Fixed a non canonical CRAN URL in the README file.

### Changes for version 0.0.5
* `partial` now makes sure each column of `pred.grid` has the correct class, levels, etc.
* `partial` gained a new option, `levelplot`, which defaults to `TRUE`. The original option, `contour`, has changed and now specifies whether or not to add contour lines whenever `levelplot = TRUE`.

### Changes for version 0.0.4
* Fixed a number of URLs.
* More thorough documentation.

### Changes for version 0.0.2

* Fixed a couple of URLs and typos.
* Added more thorough documentation.
* Added support for C5.0, Cubist, nonlinear least squares, and XGBoost models.

### Changes for version 0.0.1

* Initial release.
