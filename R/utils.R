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
superType.lm <- function(object) {
  "regression"
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
  } else if (object$method == "classification") {
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


