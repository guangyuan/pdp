#' @keywords internal
pdClassification <- function(object, pred.var, pred.grid, which.class,
                             train, ...) {
  UseMethod("pdClassification")
}


#' @keywords internal
pdClassification.default <- function(object, pred.var, pred.grid, which.class,
                                     train, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- stats::predict(object, newdata = temp, type = "prob")
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.glm <- function(object, pred.var, pred.grid, which.class,
                                     train, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- stats::predict(object, newdata = temp, type = "response")
    # Binary regression returns a vector of predicted probabilities!
    avgLogit(cbind(pr, 1 - pr), which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.bagging <- function(object, pred.var, pred.grid, which.class,
                                      train, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- stats::predict(object, newdata = temp)$prob
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.boosting <- function(object, pred.var, pred.grid, which.class,
                                      train, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- stats::predict(object, newdata = temp)$prob
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdRegression.gbm <- function(object, pred.var, pred.grid, train,
                             ...) {
  # Necessary to avoid silly printing from predict.gbm
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    log <- utils::capture.output(pr <- stats::predict(object, newdata = temp,
                                                      type = "raw"))
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.ksvm <- function(object, pred.var, pred.grid, which.class,
                                  train, ...) {
  if (is.null(object@kcall$prob.model)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- kernlab::predict(object, newdata = temp, type = "probabilities")
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.nnet <- function(object, pred.var, pred.grid, which.class,
                                  train, ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- if (inherits(object, "multinom")) {
      stats::predict(object, newdata = temp, type = "probs")
    } else {
      stats::predict(object, newdata = temp, type = "raw")
    }
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.svm <- function(object, pred.var, pred.grid, which.class,
                                 train, ...) {
  if (is.null(object$call$probability)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    pr <- attr(stats::predict(object, newdata = temp, probability = TRUE),
               which = "probabilities")
    avgLogit(pr, which.class = which.class)
  }, ...)
}
