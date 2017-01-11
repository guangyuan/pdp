# NEWS for pdp package

### Changes for version 0.4.1
* Added support for `MASS::lda`, `MASS::qda`, and `mda::mars`.
* New arguments `quantiles`, `probs`, and `trim.outliers` in `partial`. These arguments make it easier to construct PDPs over the relevant range of a numeric predictor without having to specify `pred.grid`, especially when outliers are present in the predictors (which can distort the plotted relationship).

### Changes for version 0.4.0
* `partial` is now much faster with `"gbm"` object due to a call to `gbm::plot.gbm` whenever `pred.grid` is not explicitly given by the user. (`gbm::plot.gbm` exploits a computational shortcut that does not involve any passes over the training data.)
* New (experimental) function `topPredictors` for extracting the names of the most "important" predictors. This should make it one step easier (in most cases) to construct PDPs for the most "important"" features in a fitted model.
* A new argument, `pred.fun`, allows the user to spply their own prediction function. Hence, it is poosible to obtain PDPs based on the median, rather than the mean. It is also possible to obtain PDPs for classification problems on the probability scale. See `?partial` for examples.
* Minor bug fixes and documentation tweaks.

### Changes for version 0.3.0
* The `...` argument in the call to `partial` now refers to additional arguments to be passed onto `stats::predict` rather than `plyr::aaply`. For example, using `partial` with `"gbm"` objects will require specification of `n.trees` which can now simply be passed to `partial` via the `...` argument.
* Added the following arguments to `partial`: `progress` (`plyr`-based progress bars), `parallel` (`plyr`/`foreach`-based parallel execution), and `paropts` (list of additional arguments passed onto `foreach` when `parallel = TRUE`).
* Various bug fixes.
* `partial` now throws an informative error message when the `pred.grid` argument refers to predictors not in the original training data.
* The column name for the predicted value has been changed from `"y"` to `"yhat"`.

### Changes for version 0.2.0
* `randomForest` is no longer imported.
* Added support for the `caret` package (i.e., objects of class `"train"`).
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
