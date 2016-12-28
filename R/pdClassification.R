#' @keywords internal
pdClassification <- function(object, pred.var, pred.grid, pred.fun, which.class,
                             train, progress, parallel, paropts, ...) {
  UseMethod("pdClassification")
}


#' @keywords internal
pdClassification.default <- function(object, pred.var, pred.grid, pred.fun,
                                     which.class, train,
                                     progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pr <- stats::predict(object, newdata = temp, type = "prob", ...)
      avgLogit(pr, which.class = which.class)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      stats::setNames(out, paste0("yhat.", 1L:length(out)))
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.glm <- function(object, pred.var, pred.grid, pred.fun,
                                 which.class, train,
                                 progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pr <- stats::predict(object, newdata = temp, type = "response", ...)
      avgLogit(cbind(pr, 1 - pr), which.class = which.class)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      stats::setNames(out, paste0("yhat.", 1L:length(out)))
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.bagging <- function(object, pred.var, pred.grid, pred.fun,
                                     which.class, train,
                                     progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pr <- stats::predict(object, newdata = temp, ...)$prob
      avgLogit(pr, which.class = which.class)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      stats::setNames(out, paste0("yhat.", 1L:length(out)))
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.boosting <- function(object, pred.var, pred.grid, pred.fun,
                                      which.class, train,
                                      progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pr <- stats::predict(object, newdata = temp, ...)$prob
      avgLogit(pr, which.class = which.class)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      stats::setNames(out, paste0("yhat.", 1L:length(out)))
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.gbm <- function(object, pred.var, pred.grid, pred.fun,
                                 which.class, train,
                                 progress, parallel, paropts, ...) {
  # Necessary to avoid silly printing from predict.gbm
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      # log <- utils::capture.output(
      #   pr <- stats::predict(object, newdata = temp, type = "response", ...)
      # )
      pr <- stats::predict(object, newdata = temp, type = "response", ...)
      avgLogit(cbind(pr, 1 - pr), which.class = which.class)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      stats::setNames(out, paste0("yhat.", 1L:length(out)))
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.xgb.Booster <- function(object, pred.var, pred.grid, pred.fun,
                                         which.class, train,
                                         progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pr <- stats::predict(object, newdata = data.matrix(temp), ...)
      if (object$params$objective == "binary:logistic") {
        pr <- cbind(pr, 1 - pr)
      } else {
        dim(pr) <- c(nrow(train), object$params$num_class)  # reshape into matrix
      }
      avgLogit(pr, which.class = which.class)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      stats::setNames(out, paste0("yhat.", 1L:length(out)))
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.ksvm <- function(object, pred.var, pred.grid, pred.fun,
                                  which.class, train,
                                  progress, parallel, paropts, ...) {
  if (is.null(object@kcall$prob.model)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pr <- kernlab::predict(object, newdata = temp, type = "probabilities", ...)
      avgLogit(pr, which.class = which.class)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      stats::setNames(out, paste0("yhat.", 1L:length(out)))
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.nnet <- function(object, pred.var, pred.grid, pred.fun,
                                  which.class, train,
                                  progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pr <- if (inherits(object, "multinom")) {
        stats::predict(object, newdata = temp, type = "probs", ...)
      } else {
        stats::predict(object, newdata = temp, type = "raw", ...)
      }
      avgLogit(pr, which.class = which.class)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      stats::setNames(out, paste0("yhat.", 1L:length(out)))
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.svm <- function(object, pred.var, pred.grid, pred.fun,
                                 which.class, train,
                                 progress, parallel, paropts, ...) {
  if (is.null(object$call$probability)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    out <- if (is.null(pred.fun)) {
      pr <- attr(stats::predict(object, newdata = temp, probability = TRUE,
                                ...), which = "probabilities")
      avgLogit(pr, which.class = which.class)
    } else {
      pred.fun(object, newdata = temp)
    }
    if (length(out) == 1) {
      stats::setNames(out, "yhat")
    } else {
      stats::setNames(out, paste0("yhat.", 1L:length(out)))
    }
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}
