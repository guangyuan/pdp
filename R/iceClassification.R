#' @keywords internal
iceClassification <- function(object, pred.var, pred.grid, pred.fun,
                              which.class, prob, train, progress, parallel,
                              paropts, ...) {
  if (prob) {  # return partial dependence function on probability scale
    plyr::adply(pred.grid, .margins = 1, .progress = progress,
                .parallel = parallel, .paropts = paropts,
                .fun = function(x) {
                  temp <- train
                  temp[, pred.var] <- x
                  stats::setNames(  # return averaged predicted probabiliy
                    getIceClassProb(object, newdata = temp,
                                    which.class = which.class, ...), "yhat"
                  )
                }, .id = NULL)
  } else {  # return partial dependence function on centered logit scale
    plyr::adply(pred.grid, .margins = 1, .progress = progress,
                .parallel = parallel, .paropts = paropts,
                .fun = function(x) {
                  temp <- train
                  temp[, pred.var] <- x
                  stats::setNames(  # return averaged centered logit
                    getIceClassLogit(object, newdata = temp,
                                     which.class = which.class, ...), "yhat"
                  )
                }, .id = NULL)
  }
}


#' @keywords internal
getIceClassLogit <- function(object, newdata, which.class, ...) {
  UseMethod("getIceClassLogit")
}


#' @keywords internal
getIceClassProb <- function(object, newdata, which.class, ...) {
  UseMethod("getIceClassProb")
}


#' @keywords internal
getIceClassLogit.default <- function(object, newdata, which.class, ...) {
  multiClassLogit(stats::predict(object, newdata = newdata, type = "prob", ...),
                  which.class = which.class)
}


#' @keywords internal
getIceClassProb.default <- function(object, newdata, which.class, ...) {
  stats::predict(object, newdata = newdata, type = "prob", ...)[, which.class]
}


#' @keywords internal
getIceClassLogit.BinaryTree <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  multiClassLogit(do.call(rbind, pr), which.class = which.class)
}


#' @keywords internal
getIceClassProb.BinaryTree <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  do.call(rbind, pr)[, which.class]
}


#' @keywords internal
getIceClassLogit.bagging <- function(object, newdata, which.class, ...) {
  multiClassLogit(stats::predict(object, newdata = newdata, ...)$prob,
                  which.class = which.class)
}


#' @keywords internal
getIceClassProb.bagging <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$prob[, which.class]
}


#' @keywords internal
getIceClassLogit.boosting <- function(object, newdata, which.class, ...) {
  multiClassLogit(stats::predict(object, newdata = newdata, ...)$prob,
                  which.class = which.class)
}


#' @keywords internal
getIceClassProb.boosting <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$prob[, which.class]
}


#' @keywords internal
getIceClassLogit.earth <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  multiClassLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
getIceClassProb.earth <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  cbind(pr, 1 - pr)[, which.class]
}


#' @keywords internal
getIceClassLogit.gbm <- function(object, newdata, which.class, ...) {
  invisible(utils::capture.output(
    pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  ))
  multiClassLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
getIceClassProb.gbm <- function(object, newdata, which.class, ...) {
  invisible(utils::capture.output(
    pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  ))
  cbind(pr, 1 - pr)[, which.class]
}


#' @keywords internal
getIceClassLogit.glm <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  multiClassLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
getIceClassProb.glm <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  cbind(pr, 1 - pr)[, which.class]
}


#' @keywords internal
getIceClassLogit.ksvm <- function(object, newdata, which.class, ...) {
  if (is.null(object@kcall$prob.model)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  multiClassLogit(kernlab::predict(object, newdata = newdata,
                                   type = "probabilities", ...),
                  which.class = which.class)
}


#' @keywords internal
getIceClassProb.ksvm <- function(object, newdata, which.class, ...) {
  if (is.null(object@kcall$prob.model)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  kernlab::predict(object, newdata = newdata, type = "probabilities",
                   ...)[, which.class]
}


#' @keywords internal
getIceClassLogit.lda <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$posterior
  multiClassLogit(pr, which.class = which.class)
}


#' @keywords internal
getIceClassProb.lda <- function(object, newdata, which.class, ...) {
  stats::predict(object, newdata = newdata, ...)$posterior[, which.class]
}


#' @keywords internal
getIceClassLogit.nnet <- function(object, newdata, which.class, ...) {
  pr <- if (inherits(object, "multinom")) {
    stats::predict(object, newdata = newdata, type = "probs", ...)
  } else {
    stats::predict(object, newdata = newdata, type = "raw", ...)
  }
  # It seems that when the response has more than two levels, predict.nnet
  # returns a matrix whose column names are the same as the factor levels. When
  # the response is binary, a single-columned matrix with no column name is
  # returned.
  if (ncol(pr) == 1) {
    multiClassLogit(cbind(pr, 1 - pr), which.class = which.class)
  } else {
    multiClassLogit(pr, which.class = which.class)
  }
}


#' @keywords internal
getIceClassProb.nnet <- function(object, newdata, which.class, ...) {
  pr <- if (inherits(object, "multinom")) {
    stats::predict(object, newdata = newdata, type = "probs", ...)
  } else {
    stats::predict(object, newdata = newdata, type = "raw", ...)
  }
  # It seems that when the response has more than two levels, predict.nnet
  # returns a matrix whose column names are the same as the factor levels. When
  # the response is binary, a single-columned matrix with no column name is
  # returned.
  if (ncol(pr) == 1) {
    cbind(pr, 1 - pr)[, which.class]
  } else {
    pr[, which.class]
  }
}


#' @keywords internal
getIceClassLogit.qda <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$posterior
  multiClassLogit(pr, which.class = which.class)
}


#' @keywords internal
getIceClassProb.qda <- function(object, newdata, which.class, ...) {
  stats::predict(object, newdata = newdata, ...)$posterior[, which.class]
}



#' @keywords internal
getIceClassLogit.RandomForest <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  multiClassLogit(do.call(rbind, pr), which.class = which.class)
}


#' @keywords internal
getIceClassProb.RandomForest <- function(object, newdata, which.class,
                                        ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  do.call(rbind, pr)[, which.class]
}


#' @keywords internal
getIceClassLogit.ranger <- function(object, newdata, which.class, ...) {
  if (object$treetype != "Probability estimation") {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  multiClassLogit(stats::predict(object, data = newdata, ...)$predictions,
                  which.class = which.class)
}


#' @keywords internal
getIceClassProb.ranger <- function(object, newdata, which.class, ...) {
  if (object$treetype != "Probability estimation") {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  stats::predict(object, data = newdata, ...)$predictions[, which.class]
}


#' @keywords internal
getIceClassLogit.svm <- function(object, newdata, which.class, ...) {
  if (is.null(object$call$probability)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  multiClassLogit(attr(stats::predict(object, newdata = newdata,
                                      probability = TRUE, ...),
                       which = "probabilities"), which.class = which.class)
}


#' @keywords internal
getIceClassProb.svm <- function(object, newdata, which.class, ...) {
  if (is.null(object$call$probability)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  attr(stats::predict(object, newdata = newdata, probability = TRUE, ...),
       which = "probabilities")[, which.class]
}


#' @keywords internal
getIceClassLogit.xgb.Booster <- function(object, newdata, which.class,
                                        ...) {
  pr <- stats::predict(object, newdata = newdata, reshape = TRUE, ...)
  if (object$params$objective == "binary:logistic") {
    pr <- cbind(pr, 1 - pr)
  }
  multiClassLogit(pr, which.class = which.class)
}


#' @keywords internal
getIceClassProb.xgb.Booster <- function(object, newdata, which.class,
                                       ...) {
  pr <- stats::predict(object, newdata = newdata, reshape = TRUE, ...)
  if (object$params$objective == "binary:logistic") {
    pr <- cbind(pr, 1 - pr)
  }
  pr[, which.class]
}
