#' @keywords internal
mean.inv.link <- function(x, inv.link) {
  if (anyNA(x)) {
    mean(inv.link(x[!is.na(x)]))
  } else {
    mean(inv.link(x))
  }

}


#' @keywords internal
pdInvLinkRegression <- function(object, pred.var, pred.grid, pred.fun, inv.link,
                                train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .progress = progress,
              .parallel = parallel, .paropts = paropts,
              .fun = function(x) {
                temp <- train
                temp[, pred.var] <- x
                stats::setNames(
                  getPDRegInvLinkPred(object, newdata = temp,
                                      inv.link = inv.link, ...), "yhat"
                )
              }, .id = NULL)
}


#' @keywords internal
getPDRegInvLinkPred <- function(object, newdata, inv.link, ...) {
  UseMethod("getPDRegInvLinkPred")
}


#' @keywords internal
getPDRegInvLinkPred.default <- function(object, newdata, inv.link, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  if (is.matrix(pred) || is.data.frame(pred)) {
    pred <- pred[, 1L, drop = TRUE]
  }
  mean.inv.link(pred, inv.link = inv.link)
}


#' @keywords internal
getPDRegInvLinkPred.gbm <- function(object, newdata, inv.link, ...) {
  invisible(utils::capture.output(
    pred <- stats::predict(object, newdata = newdata, ...)
  ))
  mean.inv.link(pred, inv.link = inv.link)
}


#' @keywords internal
getPDRegInvLinkPred.xgb.Booster <- function(object, newdata, inv.link, ...) {
  pred <- stats::predict(object, newdata = newdata, ...)
  mean.inv.link(pred, inv.link = inv.link)
}
