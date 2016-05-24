#' Partial Dependence (Two Variables)
#'
#' One- and two-dimensional partial dependence plots for objects of class
#' \code{mertree}.
#'
#' @param object A \code{mertree} object.
#' @param x1.name Character string giving the name of the first independent
#'   variable of interest.
#' @param x2.name Character string giving the name of the second independent
#'   variable of interest.
#' @param n1 Integer giving the number of unique data points to use in
#'   computing the partial dependence values for \code{x1.name}.
#' @param n2 Integer giving the number of unique data points to use in
#'   computing the partial dependence values for \code{x2.name}.
#' @param x1.class Character string specifying the class for \code{x1.name}.
#' @param x2.class Character string specifying the class for \code{x2.name}.
#' @param newdata An optional data frame.
#' @param ... Additional optional arguments passed onto \code{aaply}.
#' @note
#' It may be necessary to supply values for \code{x1.class} or \code{x2.class}
#' when \code{n1} or \code{n2}, respectively, are supplied. This is usually the
#' case when \code{object} is of class \code{"BinaryTree"} or \code{"mertree"}
#' (with \code{unbiased = TRUE}).
#' @importFrom plyr adply
#' @export
partial_2d <- function(object, ...) {
  UseMethod("partial_1d")
}


partial_2d.default <- function(object, x1.name, x2.name, n1, n2, 
                               x1.values, x2.values, x1.class, x2.class, 
                               newdata, ...) {
  
  # Data frame
  newdata <- if (missing(newdata)) eval(object$call$data) else newdata
  
  x1.class <- class(newdata[[x1.name]])
  x2.class <- class(newdata[[x2.name]])
  
  # First predictor values of interest
  x1.values <- if (missing(x1.values)) {
    if (x1.class == "factor") {
      levels(newdata[[x1.name]])
    } else if (missing(n1)) {
      sort(unique(newdata[[x1.name]]))
    } else {
      seq(from = min(newdata[[x1.name]]), 
          to = max(newdata[[x1.name]]), length = n1)
    }
  }
  
  # Second predictor values of interest
  x2.values <- if (missing(x2.values)) {
    if (x1.class == "factor") {
      levels(newdata[[x2.name]])
    } else if (missing(n2)) {
      sort(unique(newdata[[x2.name]]))
    } else {
      seq(from = min(newdata[[x2.name]]), 
          to = max(newdata[[x2.name]]), length = n2)
    }
  }
  
  # Data frame of unique combinations
  xgrid <- expand.grid("x1" = x1.values, "x2" = x2.values)
  
  # Determine the type of supervised learning used
  if (missing(super.type)) {
    super.type <- superType(object)
  } else {
    if (!(super.type %in% c("regression", "classification"))) {
      stop("Only regression and classification are supported.")
    }
  }
  
  # Make sure x1 and x2 have the correct class
  if (!missing(x1.class)) {
    class(xgrid$x1) <- x1.class
  }
  if (!missing(x2.class)) {
    class(xgrid$x2) <- x2.class
  }
  
  # Calculate partial dependence values
  if (super.type == "regression") {
    pd_df <- adply(xgrid, .margins = 1, .fun = function(x) {
      temp <- newdata
      temp[[x1.name]] <- x[[1L]]
      temp[[x2.name]] <- x[[2L]]
      mean(predict(object, newdata = temp), na.rm = TRUE)
    }, ...)
  } else if (super.type == "classification") {
    pd_df <- adply(xgrid, .margins = 1, .fun = function(x) {
      temp <- newdata
      temp[[x1.name]] <- x[[1L]]
      temp[[x2.name]] <- x[[2L]]
      pr <- predict(object, newdata = temp, type = "prob")
      avgLogit(pr, which.class = which.class)
    }, ...)
  } else {
    stop(paste("Partial dependence values are currently only available",
               "for classification and regression problems."))
  }
  
  # Return data frame of partial dependence values
  names(pd_df) <- c(x1.name, x2.name, "y")
  class(pd_df) <- c("data.frame", "partial_2d")
  pd_df
  
}
