#' @keywords internal
getIcePredReg <- function(object, newdata, ...) {
  UseMethod("getIcePredReg")
}


#' @keywords internal
getIcePredReg.default <- function(object, newdata, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  if (is.matrix(pred) || is.data.frame(pred)) {
    pred[, 1L, drop = TRUE]
  } else {
    pred
  }
}


#' @keywords internal
getIcePredReg.gbm <- function(object, newdata, ...) {
  invisible(utils::capture.output(
    pred <- stats::predict(object, newdata = newdata, ...)
  ))
  pred
}


#' @keywords internal
getIcePredReg.ksvm <- function(object, newdata, ...) {
  kernlab::predict(object, newdata = newdata, ...)[, 1L, drop = TRUE]
}


#' @keywords internal
getIcePredReg.mars <- function(object, newdata, ...) {
  stats::predict(object,
                 newdata = data.matrix(newdata), ...)[, 1L, drop = TRUE]
}


#' @keywords internal
getIcePredReg.ranger <- function(object, newdata, ...) {
  stats::predict(object, data = newdata, ...)$predictions
}


#' @keywords internal
getIcePredReg.xgb.Booster <- function(object, newdata, ...) {
  stats::predict(object, newdata = newdata, ...)
}


#' @keywords internal
getIcePredRegInvLink <- function(object, newdata, inv.link, ...) {
  UseMethod("getIcePredRegInvLink")
}


#' @keywords internal
getIcePredRegInvLink.default <- function(object, newdata, inv.link, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  if (is.matrix(pred) || is.data.frame(pred)) {
    pred <- pred[, 1L, drop = TRUE]
  }
  inv.link(pred)
}


#' @keywords internal
getIcePredRegInvLink.gbm <- function(object, newdata, inv.link, ...) {
  invisible(utils::capture.output(
    pred <- stats::predict(object, newdata = newdata, ...)
  ))
  inv.link(pred)
}


#' @keywords internal
getIcePredRegInvLink.xgb.Booster <- function(object, newdata, inv.link, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  inv.link(pred)
}
