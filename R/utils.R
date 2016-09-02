#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' @keywords internal
avgLogit <- function(x, which.class = 1L) {
  stopifnot(is.matrix(x))  # x should be a nclass by n probability matrix
  eps <- .Machine$double.eps
  mean(log(ifelse(x[, which.class] > 0, x[, which.class], eps)) -
         rowMeans(log(ifelse(x > 0, x, eps))), na.rm = TRUE)
}


#' @keywords internal
superType <- function(object) {
  UseMethod("superType")
}


#' @keywords internal
superType.bagging <- function(object) {
  "classification"
}


#' @keywords internal
superType.boosting <- function(object) {
  "classification"
}


#' @keywords internal
superType.earth <- function(object) {
  if (!is.null(object$glm.list) &&
      object$glm.list[[1L]]$family$family == "binomial") {
    "classification"
  } else if (is.null(object$glm.list) ||
             object$glm.list[[1L]]$family$family == "gaussian") {
    "regression"
  } else {
    "other"
  }
}


#' @keywords internal
superType.lm <- function(object) {
  # FIXME: What about multivariate response models?
  "regression"
}


#' @keywords internal
superType.nls <- function(object) {
  "regression"
}


#' @keywords internal
superType.glm <- function(object) {
  if(object$family$family == "binomial") {
    "classification"
  } else {
    "other"
  }
}


#' @keywords internal
superType.multinom <- function(object) {
  # FIXME: What about multivariate response models?
  "classification"
}


#' @keywords internal
superType.rpart <- function(object) {
  if (object$method == "anova") {
    "regression"
  } else if (object$method == "class") {
    "classification"
  } else {
    "other"
  }
}


#' @keywords internal
superType.BinaryTree <- function(object) {
  if (object@responses@is_nominal) {
    "classification"
  } else if (object@responses@is_ordinal || object@responses@is_censored) {
    "other"
  } else {
    "regression"
  }
}


#' @keywords internal
superType.randomForest <- function(object) {
  if (object$type == "regression") {
    "regression"
  } else if (object$type == "classification") {
    "classification"
  } else {
    "unsupervised"
  }
}


#' @keywords internal
superType.RandomForest <- function(object) {
  if (object@responses@is_nominal) {
    "classification"
  } else if (object@responses@is_ordinal || object@responses@is_censored) {
    "other"
  } else {
    "regression"
  }
}


#' @keywords internal
superType.gbm <- function(object) {
  if (object$distribution %in% c("gaussian", "laplace", "tdist")) {
    "regression"
  } else if (object$distribution %in% c("bernoulli", "huberized", "multinomial",
                                        "adaboost")) {
    "classification"
  } else {
    "other"
  }
}


#' @keywords internal
superType.xgb.Booster <- function(object) {
  if (object$params$objective == "reg:linear") {
    "regression"
  } else if (object$params$objective %in%
             c("binary:logistic", "multi:softprob")) {
    # FIXME: Throw a warning if objective function is classification, but does
    # not return the predicted probabilities (e.g., "binary:logitraw").
    "classification"
  } else if (object$params$objective %in%
             c("reg:logistic", "binary:logitraw", "multi:softmax")) {
    stop(paste0("For classification, switch to an objective function",
                "that returns the predicted probabilities."))
  } else {
    "other"
  }
}


#' @keywords internal
superType.svm <- function(object) {
  if (is.null(object$levels)) {
    "regression"
  } else {
    "classification"
  }
}


#' @keywords internal
superType.ksvm <- function(object) {
  if (grepl("svr$", object@type)) {
    "regression"
  } else if (grepl("svc$", object@type)) {
    "classification"
  } else {
    "other"
  }
}
