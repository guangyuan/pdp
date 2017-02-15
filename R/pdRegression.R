#' @keywords internal
pdRegression <- function(object, pred.var, pred.grid, pred.fun,
                         train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .progress = progress,
              .parallel = parallel, .paropts = paropts,
              .fun = function(x) {
                temp <- train
                temp[, pred.var] <- x
                stats::setNames(getPDRegPred(object, newdata = temp, ...),
                                "yhat")
              }, .id = NULL)
}


#' @keywords internal
getPDRegPred <- function(object, newdata, ...) {
  UseMethod("getPDRegPred")
}


#' @keywords internal
getPDRegPred.default <- function(object, newdata, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  if (is.matrix(pred) || is.data.frame(pred)) {
    pred <- pred[, 1L, drop = TRUE]
  }
  mean(pred, na.rm = TRUE)
}


#' @keywords internal
getPDRegPred.gbm <- function(object, newdata, ...) {
  invisible(utils::capture.output(
    pred <- stats::predict(object, newdata = newdata, ...)
  ))
  mean(pred, na.rm = TRUE)
}


#' @keywords internal
getPDRegPred.ksvm <- function(object, newdata, ...) {
  mean(kernlab::predict(object, newdata = newdata, ...)[, 1L, drop = TRUE],
       na.rm = TRUE)
}


#' @keywords internal
getPDRegPred.mars <- function(object, newdata, ...) {
  mean(stats::predict(object,
                      newdata = data.matrix(newdata), ...)[, 1L, drop = TRUE],
       na.rm = TRUE)
}


#' @keywords internal
getPDRegPred.ranger <- function(object, newdata, ...) {
  mean(stats::predict(object, data = newdata, ...)$predictions, na.rm = TRUE)
}


#' @keywords internal
getPDRegPred.xgb.Booster <- function(object, newdata, ...) {
  mean(stats::predict(object, newdata = newdata, ...),
       na.rm = TRUE)
}
