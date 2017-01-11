#' @keywords internal
pdClassification <- function(object, pred.var, pred.grid, pred.fun, which.class,
                             train, progress, parallel, paropts, ...) {

  # Use plyr::adply, rather than a for loop
  plyr::adply(pred.grid, .margins = 1, .progress = progress,
              .parallel = parallel, .paropts = paropts, .fun = function(x) {

    # Copy training data and replace pred.var with constant
    temp <- train
    temp[pred.var] <- x

    # Get prediction(s)
    if (is.null(pred.fun)) {
      stats::setNames(pdPredictClassification(object, newdata = temp,
                                              which.class = which.class, ...),
                      "yhat")
    } else {
      out <- pred.fun(object, newdata = temp)
      if (length(out) == 1) {
        stats::setNames(out, "yhat")
      } else {
        if (is.null(names(out))) {
          stats::setNames(out, paste0("yhat.", 1L:length(out)))
        } else {
          stats::setNames(out, paste0("yhat.", names(out)))
        }
      }
    }

  })

}


#' @keywords internal
pdPredictClassification <- function(object, newdata, which.class, ...) {
  UseMethod("pdPredictClassification")
}


#' @keywords internal
pdPredictClassification.default <- function(object, newdata, which.class, ...) {
  avgLogit(stats::predict(object, newdata = newdata, type = "prob", ...),
           which.class = which.class)
}


#' @keywords internal
pdPredictClassification.BinaryTree <- function(object, newdata, which.class,
                                               ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  avgLogit(do.call(rbind, pr), which.class = which.class)
}


#' @keywords internal
pdPredictClassification.bagging <- function(object, newdata, which.class, ...) {
  avgLogit(stats::predict(object, newdata = newdata, ...)$prob,
           which.class = which.class)
}


#' @keywords internal
pdPredictClassification.boosting <- function(object, newdata, which.class,
                                             ...) {
  avgLogit(stats::predict(object, newdata = newdata, ...)$prob,
           which.class = which.class)
}


#' @keywords internal
pdPredictClassification.earth <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
pdPredictClassification.gbm <- function(object, newdata, which.class, ...) {
  invisible(capture.output(
    pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  ))
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
pdPredictClassification.glm <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "response", ...)
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
pdPredictClassification.ksvm <- function(object, newdata, which.class, ...) {
  if (is.null(object@kcall$prob.model)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  avgLogit(kernlab::predict(object, newdata = newdata,
                            type = "probabilities", ...),
           which.class = which.class)
}


#' @keywords internal
pdPredictClassification.lda <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$posterior
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
pdPredictClassification.nnet <- function(object, newdata, which.class, ...) {
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
pdPredictClassification.qda <- function(object, newdata, which.class, ...) {
  pr <- stats::predict(object, newdata = newdata, ...)$posterior
  avgLogit(cbind(pr, 1 - pr), which.class = which.class)
}


#' @keywords internal
pdPredictClassification.RandomForest <- function(object, newdata, which.class,
                                                 ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  avgLogit(do.call(rbind, pr), which.class = which.class)
}


#' @keywords internal
pdPredictClassification.ranger <- function(object, newdata, which.class, ...) {
  if (object$treetype != "Probability estimation") {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  avgLogit(stats::predict(object, data = newdata, ...)$predictions,
           which.class = which.class)
}


#' @keywords internal
pdPredictClassification.svm <- function(object, newdata, which.class, ...) {
  if (is.null(object$call$probability)) {
    stop(paste("Cannot obtain predicted probabilities from",
               deparse(substitute(object))))
  }
  avgLogit(attr(stats::predict(object, newdata = newdata, probability = TRUE,
                               ...),
                which = "probabilities"), which.class = which.class)
}


#' @keywords internal
pdPredictClassification.xgb.Booster <- function(object, newdata, which.class,
                                                ...) {
  pr <- stats::predict(object, newdata = data.matrix(newdata), ...)
  if (object$params$objective == "binary:logistic") {
    pr <- cbind(pr, 1 - pr)
  } else {
    dim(pr) <- c(nrow(newdata), object$params$num_class)  # reshape into matrix
  }
  avgLogit(pr, which.class = which.class)
}
