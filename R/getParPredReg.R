#' @keywords internal
getParPredReg <- function(object, newdata, ...) {
  UseMethod("getParPredReg")
}


#' @keywords internal
getParPredReg.default <- function(object, newdata, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  if (is.matrix(pred) || is.data.frame(pred)) {
    pred <- pred[, 1L, drop = TRUE]
  }
  mean(pred, na.rm = TRUE)
}


#' @keywords internal
getParPredReg.gbm <- function(object, newdata, ...) {
  invisible(utils::capture.output(
    pred <- stats::predict(object, newdata = newdata, ...)
  ))
  mean(pred, na.rm = TRUE)
}


#' @keywords internal
getParPredReg.ksvm <- function(object, newdata, ...) {
  mean(kernlab::predict(object, newdata = newdata, ...)[, 1L, drop = TRUE],
       na.rm = TRUE)
}


#' @keywords internal
getParPredReg.mars <- function(object, newdata, ...) {
  mean(stats::predict(object,
                      newdata = data.matrix(newdata), ...)[, 1L, drop = TRUE],
       na.rm = TRUE)
}


#' @keywords internal
getParPredReg.ranger <- function(object, newdata, ...) {
  mean(stats::predict(object, data = newdata, ...)$predictions, na.rm = TRUE)
}


#' @keywords internal
getParPredReg.xgb.Booster <- function(object, newdata, ...) {
  mean(stats::predict(object, newdata = newdata, ...),
       na.rm = TRUE)
}


#' @keywords internal
getParPredRegInvLink <- function(object, newdata, inv.link, ...) {
  UseMethod("getParPredRegInvLink")
}


#' @keywords internal
getParPredRegInvLink.default <- function(object, newdata, inv.link, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  if (is.matrix(pred) || is.data.frame(pred)) {
    pred <- pred[, 1L, drop = TRUE]
  }
  pred <- pred[!is.na(pred)]
  mean(inv.link(pred))
}


#' @keywords internal
getParPredRegInvLink.gbm <- function(object, newdata, inv.link, ...) {
  invisible(utils::capture.output(
    pred <- stats::predict(object, newdata = newdata, ...)
  ))
  pred <- pred[!is.na(pred)]
  mean(inv.link(pred))
}


#' @keywords internal
getParPredRegInvLink.ksvm <- function(object, newdata, ...) {
  pred <- kernlab::predict(object, newdata = newdata, ...)[, 1L, drop = TRUE]
  pred <- pred[!is.na(pred)]
  mean(inv.link(pred))
}


#' @keywords internal
getParPredRegInvLink.mars <- function(object, newdata, ...) {
  pred <- stats::predict(object, newdata = data.matrix(newdata),
                         ...)[, 1L, drop = TRUE]
  pred <- pred[!is.na(pred)]
  mean(inv.link(pred))
}


#' @keywords internal
getParPredRegInvLink.ranger <- function(object, newdata, ...) {
  pred <- stats::predict(object, data = newdata, ...)$predictions
  pred <- pred[!is.na(pred)]
  mean(inv.link(pred))
}


#' @keywords internal
getParPredRegInvLink.xgb.Booster <- function(object, newdata, inv.link, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  pred <- pred[!is.na(pred)]
  mean(inv.link(pred))
}
