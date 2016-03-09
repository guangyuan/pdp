#' Partial Dependence (One Variable)
#'
#' One- and two-dimensional partial dependence plots for objects of class
#' \code{mertree}.
#'
#' @param object A \code{mertree} object.
#' @param x.name Character string giving the name of the independent variable of
#'   interest.
#' @param n Integer giving the number of unique data points to use in
#'   computing the partial dependence values.
#' @param x.class Character string specifying the class for \code{x.name}.
#' @param newdata An optional data frame.
#' @param type Character string
#' @param ... Additional optional arguments passed onto \code{aaply}.
#' @note
#' It may be necessary to supply a value for \code{x.class} when \code{n} is
#' supplied. This is usually the case when \code{object} is of class
#' \code{"BinaryTree"} or \code{"mertree"} (with \code{unbiased = TRUE}).
#' @importFrom plyr laply
#' @export
partial_1d <- function(object, x.name, x.values, n, newdata, which.class = 1L,
                       ...) {
  UseMethod("partial_1d")
}


partial_1d.default <- function(object, x.name, x.values, n, newdata, ...) {

  # Data frame
  newdata <- if (missing(newdata)) eval(object$call$data) else newdata

  x.class <- class(newdata[[x.name]])

  # Predictor values of interest
  x.values <- if (missing(x.values)) {
    if (x.class == "factor") {
      levels(newdata[[x.name]])
    } else if (missing(n)) {
      sort(unique(newdata[[x.name]]))
    } else {
      seq(from = min(newdata[[x.name]]), to = max(newdata[[x.name]]), length = n)
    }
  }

  # Make sure x.values has the correct x.class
  class(x.values) <- x.class

  # Calculate partial dependence values
  super.type <- superType(object)
  if (super.type == "regression") {
    pd <- laply(x.values, .fun = function(x) {
      temp <- newdata
      temp[[x.name]] <- x
      mean(predict(object, newdata = temp))
    }, ...)
  } else if (super.type == "classification") {
    pd <- laply(x.values, .fun = function(x) {
      temp <- newdata
      temp[[x.name]] <- x
      pr <- predict(object, newdata = temp, type = "prob")
      avgLogit(pr, which.class = which.class)
    }, ...)
  } else {
    stop(paste("Partial dependence values are currently only available",
               "for classification and regression problems."))
  }

  # Return data frame of partial dependence values
  pd_df <- data.frame(x = x.values, y = pd)
  names(pd_df) <- c(x.name, "y")
  pd_df

}
