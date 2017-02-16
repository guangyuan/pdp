#' @keywords internal
pdClassification <- function(object, pred.var, pred.grid, pred.fun, which.class,
                             prob, train, progress, parallel, paropts, ...) {
  if (prob) {  # return partial dependence function on probability scale
    plyr::adply(pred.grid, .margins = 1, .progress = progress,
                .parallel = parallel, .paropts = paropts,
                .fun = function(x) {
                  temp <- train
                  temp[, pred.var] <- x
                  stats::setNames(  # return averaged predicted probabiliy
                    getPDClassProb(object, newdata = temp,
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
                    getPDClassLogit(object, newdata = temp,
                                    which.class = which.class, ...), "yhat"
                  )
                }, .id = NULL)
  }
}


#' @keywords internal
getPDClassLogit <- function(object, newdata, which.class, ...) {
  UseMethod("getPDClassLogit")
}


#' @keywords internal
getPDClassProb <- function(object, newdata, which.class, ...) {
  UseMethod("getPDClassProb")
}


#' @keywords internal
getPDClassLogit.default <- function(object, newdata, which.class, ...) {
  avgLogit(stats::predict(object, newdata = newdata, type = "prob", ...),
           which.class = which.class)
}


#' @keywords internal
getPDClassProb.default <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  mean(pr[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.BinaryTree <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  avgLogit(do.call(rbind, pr), which.class = which.class)
}


#' @keywords internal
getPDClassProb.BinaryTree <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  mean(do.call(rbind, pr)[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.bagging <- function(object, newdata, which.class, ...) {
  avgLogit(stats::predict(object, newdata = newdata, ...)$prob,
           which.class = which.class)
}


#' @keywords internal
getPDClassProb.bagging <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$prob
  mean(pr[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.boosting <- function(object, newdata, which.class, ...) {
  avgLogit(stats::predict(object, newdata = newdata, ...)$prob,
           which.class = which.class)
}


#' @keywords internal
getPDClassProb.boosting <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$prob
  mean(pr[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.earth <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
getPDClassProb.earth <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  mean(cbind(pr, 1 - pr)[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.gbm <- function(object, newdata, which.class, ...) {
  invisible(utils::capture.output(
    pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  ))
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
getPDClassProb.gbm <- function(object, newdata, which.class, ...) {
  invisible(utils::capture.output(
    pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  ))
  mean(cbind(pr, 1 - pr)[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.glm <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
getPDClassProb.glm <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  mean(cbind(pr, 1 - pr)[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.ksvm <- function(object, newdata, which.class, ...) {
  if (is.null(object@kcall$prob.model)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  avgLogit(kernlab::predict(object, newdata = newdata,
                            type = "probabilities", ...),
           which.class = which.class)
}


#' @keywords internal
getPDClassProb.ksvm <- function(object, newdata, which.class, ...) {
  if (is.null(object@kcall$prob.model)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  pr <- kernlab::predict(object, newdata = newdata,
                         type = "probabilities", ...)
  mean(pr[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.lda <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$posterior
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
getPDClassProb.lda <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$posterior
  mean(cbind(pr, 1 - pr)[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.nnet <- function(object, newdata, which.class, ...) {
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
    avgLogit(cbind(pr, 1 - pr), which.class = which.class)
  } else {
    avgLogit(pr, which.class = which.class)
  }
}


#' @keywords internal
getPDClassProb.nnet <- function(object, newdata, which.class, ...) {
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
    mean(cbind(pr, 1 - pr)[, which.class], na.rm = TRUE)
  } else {
    mean(pr[, which.class], na.rm = TRUE)
  }
}


#' @keywords internal
getPDClassLogit.qda <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$posterior
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
getPDClassProb.qda <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$posterior
  mean(cbind(pr, 1 - pr)[, which.class], na.rm = TRUE)
}



#' @keywords internal
getPDClassLogit.RandomForest <- function(object, newdata, which.class,
                                                 ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  avgLogit(do.call(rbind, pr), which.class = which.class)
}


#' @keywords internal
getPDClassProb.RandomForest <- function(object, newdata, which.class,
                                         ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  mean(do.call(rbind, pr)[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.ranger <- function(object, newdata, which.class, ...) {
  if (object$treetype != "Probability estimation") {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  avgLogit(stats::predict(object, data = newdata, ...)$predictions,
           which.class = which.class)
}


#' @keywords internal
getPDClassProb.ranger <- function(object, newdata, which.class, ...) {
  if (object$treetype != "Probability estimation") {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  pr <- stats::predict(object, data = newdata, ...)$predictions
  mean(pr[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.svm <- function(object, newdata, which.class, ...) {
  if (is.null(object$call$probability)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  avgLogit(attr(stats::predict(object, newdata = newdata, probability = TRUE,
                               ...),
                which = "probabilities"), which.class = which.class)
}


#' @keywords internal
getPDClassProb.svm <- function(object, newdata, which.class, ...) {
  if (is.null(object$call$probability)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  pr <- attr(stats::predict(object, newdata = newdata, probability = TRUE, ...),
             which = "probabilities")
  mean(pr[, which.class], na.rm = TRUE)
}


#' @keywords internal
getPDClassLogit.xgb.Booster <- function(object, newdata, which.class,
                                                ...) {
  pr <- stats::predict(object, newdata = newdata, ...)
  if (object$params$objective == "binary:logistic") {
    pr <- cbind(pr, 1 - pr)
  } else {
    dim(pr) <- c(nrow(newdata), object$params$num_class)  # reshape into matrix
  }
  avgLogit(pr, which.class = which.class)
}


#' @keywords internal
getPDClassProb.xgb.Booster <- function(object, newdata, which.class,
                                        ...) {
  pr <- stats::predict(object, newdata = newdata, ...)
  if (object$params$objective == "binary:logistic") {
    pr <- cbind(pr, 1 - pr)
  } else {
    dim(pr) <- c(nrow(newdata), object$params$num_class)  # reshape into matrix
  }
  mean(pr[, which.class], na.rm = TRUE)
}

