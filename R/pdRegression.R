#' @keywords internal
pdRegression <- function(object, pred.var, pred.grid, pred.fun,
                         train, progress, parallel, paropts, ...) {

  # Use plyr::adply, rather than a for loop
  plyr::adply(pred.grid, .margins = 1, .progress = progress,
              .parallel = parallel, .paropts = paropts,
              .fun = function(x) {

                # Copy training data and replace pred.var with constant
                temp <- train
                temp[pred.var] <- x

                # Get prediction(s)
                stats::setNames(pdPredictRegression(object, newdata = temp, ...), "yhat")

              })


}


#' @keywords internal
pdPredictRegression <- function(object, newdata, ...) {
  UseMethod("pdPredictRegression")
}


#' @keywords internal
pdPredictRegression.default <- function(object, newdata, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  if (is.matrix(pred) || is.data.frame(pred)) {
    pred <- pred[, 1L, drop = TRUE]
  }
  mean(pred, na.rm = TRUE)
}


#' @keywords internal
pdPredictRegression.gbm <- function(object, newdata, ...) {
  invisible(utils::capture.output(
    pred <- stats::predict(object, newdata = newdata, ...)
  ))
  mean(pred, na.rm = TRUE)
}


#' @keywords internal
pdPredictRegression.ksvm <- function(object, newdata, ...) {
  mean(kernlab::predict(object, newdata = newdata, ...)[, 1L, drop = TRUE],
       na.rm = TRUE)
}


#' @keywords internal
pdPredictRegression.mars <- function(object, newdata, ...) {
  mean(stats::predict(object,
                      newdata = data.matrix(newdata), ...)[, 1L, drop = TRUE],
       na.rm = TRUE)
}


#' @keywords internal
pdPredictRegression.ranger <- function(object, newdata, ...) {
  mean(stats::predict(object, data = newdata, ...)$predictions, na.rm = TRUE)
}


#' @keywords internal
pdPredictRegression.xgb.Booster <- function(object, newdata, ...) {
  mean(stats::predict(object, newdata = data.matrix(newdata), ...),
       na.rm = TRUE)
}
