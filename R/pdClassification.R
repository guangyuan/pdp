#' @keywords internal
pdClassification <- function(object, pred.var, pred.grid, which.class,
                             train, progress, parallel, paropts, ...) {
  UseMethod("pdClassification")
}


#' @keywords internal
pdClassification.default <- function(object, pred.var, pred.grid, which.class,
                                     train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    # FIXME: Does this copy need to be made?
    temp <- train
    temp[pred.var] <- x
    pr <- stats::predict(object, newdata = temp, type = "prob", ...)
    stats::setNames(avgLogit(pr, which.class = which.class), "y")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.glm <- function(object, pred.var, pred.grid, which.class,
                                     train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- stats::predict(object, newdata = temp, type = "response", ...)
    # Binary regression returns a vector of predicted probabilities!
    stats::setNames(avgLogit(cbind(pr, 1 - pr), which.class = which.class), "y")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.bagging <- function(object, pred.var, pred.grid, which.class,
                                      train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- stats::predict(object, newdata = temp, ...)$prob
    stats::setNames(avgLogit(pr, which.class = which.class), "y")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.boosting <- function(object, pred.var, pred.grid, which.class,
                                      train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- stats::predict(object, newdata = temp, ...)$prob
    stats::setNames(avgLogit(pr, which.class = which.class), "y")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.gbm <- function(object, pred.var, pred.grid, which.class,
                                 train, progress, parallel, paropts, ...) {
  # Necessary to avoid silly printing from predict.gbm
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    log <- utils::capture.output(pr <- stats::predict(object, newdata = temp,
                                                      type = "response", ...))
    stats::setNames(avgLogit(cbind(pr, 1 - pr), which.class = which.class), "y")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.xgb.Booster <- function(object, pred.var, pred.grid,
                                         which.class, train, progress, parallel,
                                         paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- stats::predict(object, newdata = data.matrix(temp), ...)
    if (object$params$objective == "binary:logistic") {
      pr <- cbind(pr, 1 - pr)
    } else {
      dim(pr) <- c(nrow(train), object$params$num_class)  # reshape into matrix
    }
    stats::setNames(avgLogit(pr, which.class = which.class), "y")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.ksvm <- function(object, pred.var, pred.grid, which.class,
                                  train, progress, parallel, paropts, ...) {
  if (is.null(object@kcall$prob.model)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- kernlab::predict(object, newdata = temp, type = "probabilities", ...)
    stats::setNames(avgLogit(pr, which.class = which.class), "y")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.nnet <- function(object, pred.var, pred.grid, which.class,
                                  train, progress, parallel, paropts, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- if (inherits(object, "multinom")) {
      stats::predict(object, newdata = temp, type = "probs", ...)
    } else {
      stats::predict(object, newdata = temp, type = "raw", ...)
    }
    stats::setNames(avgLogit(pr, which.class = which.class), "y")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}


#' @keywords internal
pdClassification.svm <- function(object, pred.var, pred.grid, which.class,
                                 train, progress, parallel, paropts, ...) {
  if (is.null(object$call$probability)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- attr(stats::predict(object, newdata = temp, probability = TRUE, ...),
               which = "probabilities")
    stats::setNames(avgLogit(pr, which.class = which.class), "y")
  }, .progress = progress, .parallel = parallel, .paropts = paropts)
}
