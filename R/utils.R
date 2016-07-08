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
superType.boosting <- function(object) {
  "classification"
}


#' @keywords internal
superType.lm <- function(object) {
  # FIXME: What about multivariate response models?
  "regression"
}


#' @keywords internal
superType.earth <- function(object) {
  if (is.null(object$levels)) {
    # FIXME: What about multivariate response models?
    "regression"
  } else {
    "classification"
  }
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
    "other"
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
