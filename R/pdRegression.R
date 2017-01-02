#' @keywords internal
pdRegression <- function(object, pred.var, pred.grid, pred.fun, train,
                         progress, parallel, paropts, ...) {
  UseMethod("pdRegression")
}


#' @keywords internal
pdRegression.default <- function(object, pred.var, pred.grid, pred.fun,
                                 train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pred <- stats::predict(object, newdata = temp, ...)
      if (is.matrix(pred) || is.data.frame(pred)) {
        pred <- pred[, 1L, drop = TRUE]
      }
      mean(pred, na.rm = TRUE)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      if (is.null(names(out))) {
        stats::setNames(out, paste0("yhat.", 1L:length(out)))
      } else {
        stats::setNames(out, paste0("yhat.", names(out)))
      }
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdRegression.ksvm <- function(object, pred.var, pred.grid, pred.fun,
                                 train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pred <- kernlab::predict(object, newdata = temp, ...)[, 1L, drop = TRUE]
      mean(pred, na.rm = TRUE)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      if (is.null(names(out))) {
        stats::setNames(out, paste0("yhat.", 1L:length(out)))
      } else {
        stats::setNames(out, paste0("yhat.", names(out)))
      }
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdRegression.ranger <- function(object, pred.var, pred.grid, pred.fun, train,
                                progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      mean(stats::predict(object, data = temp, ...)$predictions, na.rm = TRUE)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      if (is.null(names(out))) {
        stats::setNames(out, paste0("yhat.", 1L:length(out)))
      } else {
        stats::setNames(out, paste0("yhat.", names(out)))
      }
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdRegression.xgb.Booster <- function(object, pred.var, pred.grid, pred.fun,
                                     train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pred <- stats::predict(object, newdata = data.matrix(temp), ...)
      mean(pred, na.rm = TRUE)
    } else {
      pred.fun(object, newdata = data.matrix(temp))
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      if (is.null(names(out))) {
        stats::setNames(out, paste0("yhat.", 1L:length(out)))
      } else {
        stats::setNames(out, paste0("yhat.", names(out)))
      }
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}
