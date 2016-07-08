#' @keywords internal
pdClassification <- function(object, pred.var, pred.grid, which.class,
                             training.data, ...) {
  UseMethod("pdClassification")
}


#' @keywords internal
pdClassification.default <- function(object, pred.var, pred.grid, which.class,
                                     training.data, ...) {
  adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- training.data
    temp[pred.var] <- x
    pr <- predict(object, newdata = temp, type = "prob")
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.bagging <- function(object, pred.var, pred.grid, which.class,
                                      training.data, ...) {
  adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- training.data
    temp[pred.var] <- x
    pr <- predict(object, newdata = temp)$prob
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.boosting <- function(object, pred.var, pred.grid, which.class,
                                      training.data, ...) {
  adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- training.data
    temp[pred.var] <- x
    pr <- predict(object, newdata = temp)$prob
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
#' @importFrom utils capture.output
pdRegression.gbm <- function(object, pred.var, pred.grid, training.data,
                             ...) {
  # Necessary to avoid silly printing from predict.gbm
  adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- training.data
    temp[pred.var] <- x
    log <- capture.output(pr <- predict(object, newdata = temp, type = "raw"))
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.ksvm <- function(object, pred.var, pred.grid, which.class,
                                  training.data, ...) {
  if (is.null(object@kcall$prob.model)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- training.data
    temp[pred.var] <- x
    pr <- kernlab::predict(object, newdata = temp, type = "probabilities")
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.nnet <- function(object, pred.var, pred.grid, which.class,
                                  training.data, ...) {
  adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- training.data
    temp[pred.var] <- x
    pr <- predict(object, newdata = temp, type = "raw")
    avgLogit(pr, which.class = which.class)
  }, ...)
}


#' @keywords internal
pdClassification.svm <- function(object, pred.var, pred.grid, which.class,
                                 training.data, ...) {
  if (is.null(object$call$probability)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- training.data
    temp[pred.var] <- x
    pr <- attr(predict(object, newdata = temp, probability = TRUE),
               which = "probabilities")
    avgLogit(pr, which.class = which.class)
  }, ...)
}
