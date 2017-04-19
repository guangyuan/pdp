#' @keywords internal
iceRegression <- function(object, pred.var, pred.grid, pred.fun,
                          train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .progress = progress,
              .parallel = parallel, .paropts = paropts,
              .fun = function(x) {
                temp <- train
                temp[, pred.var] <- x
                stats::setNames(getIceRegPred(object, newdata = temp, ...),
                                "yhat")
              }, .id = NULL)
}


#' @keywords internal
getIceRegPred <- function(object, newdata, ...) {
  UseMethod("getIceRegPred")
}


#' @keywords internal
getIceRegPred.default <- function(object, newdata, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  if (is.matrix(pred) || is.data.frame(pred)) {
    pred[, 1L, drop = TRUE]
  } else {
    pred
  }
}


#' @keywords internal
getIceRegPred.gbm <- function(object, newdata, ...) {
  invisible(utils::capture.output(
    pred <- stats::predict(object, newdata = newdata, ...)
  ))
  pred
}


#' @keywords internal
getIceRegPred.ksvm <- function(object, newdata, ...) {
  kernlab::predict(object, newdata = newdata, ...)[, 1L, drop = TRUE]
}


#' @keywords internal
getIceRegPred.mars <- function(object, newdata, ...) {
  stats::predict(object, 
                 newdata = data.matrix(newdata), ...)[, 1L, drop = TRUE]
}


#' @keywords internal
getIceRegPred.ranger <- function(object, newdata, ...) {
  stats::predict(object, data = newdata, ...)$predictions
}


#' @keywords internal
getIceRegPred.xgb.Booster <- function(object, newdata, ...) {
  stats::predict(object, newdata = newdata, ...)
}
