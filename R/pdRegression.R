#' @keywords internal
pdRegression <- function(object, pred.var, pred.grid, train, progress, parallel,
                         paropts, ...) {
  UseMethod("pdRegression")
}


#' @keywords internal
pdRegression.default <- function(object, pred.var, pred.grid, train, progress,
                                 parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pred <- stats::predict(object, newdata = temp, ...)
    # Some fitting functions return a matrix (e.g., mda::mars)
    if (is.matrix(pred) || is.data.frame(pred)) {
      pred <- pred[, 1L, drop = TRUE]
    }
    stats::setNames(mean(pred, na.rm = TRUE), "yhat")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdRegression.gbm <- function(object, pred.var, pred.grid, train, progress,
                             parallel, paropts, ...) {
  # Necessary to avoid silly printing from predict.gbm
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    log <- utils::capture.output(z <- mean(stats::predict(object,
                                                          newdata = temp, ...),
                                           na.rm = TRUE))
    stats::setNames(z, "yhat")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdRegression.xgb.Booster <- function(object, pred.var, pred.grid, train,
                                     progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    stats::setNames(mean(stats::predict(object,
                                        newdata = data.matrix(temp), ...),
                         na.rm = TRUE), "yhat")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}
