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
#' @param x1.values Vector of predictor values to use in computing the partial 
#'   dependence function.
#' @param x2.values Vector of predictor values to use in computing the partial 
#'   dependence function.
#' @param super.type Character string specifying the type of supervised
#'   learning. Current options are \code{"regression"} or 
#'   \code{"classification"}. For tree-based models (e.g., \code{"rpart"}), the
#'   function can usually extract the necessary information from \code{object}.
#' @param which.class Integer specifying which class to use as the target class
#'   (classification only).
#' @param check.class Logical indicating whether or not to check the class of
#'   each predictor variable of interest.
#' @param newdata An optional data frame.
#' @param ... Additional optional arguments to be passed onto \code{aaply}.
#'
#' @importFrom plyr adply
#' @rdname partial_2d
#' @export
partial_2d <- function(object, ...) {
  UseMethod("partial_1d")
}


#' @rdname partial_2d
#' @export
partial_2d.default <- function(object, x1.name, x2.name, n1, n2, 
                               x1.values, x2.values, super.type, 
                               which.class = 1L, check.class = TRUE, 
                               newdata, ...) {
  
  # Data frame
  newdata <- if (missing(newdata)) eval(object$call$data) else newdata
  
  x1.class <- class(newdata[[x1.name]])
  x2.class <- class(newdata[[x2.name]])
  
  # First predictor values of interest
  x1.values <- if (missing(x1.values)) {
    if (is.factor(newdata[[x1.name]])) {
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
    if (is.factor(newdata[[x2.name]])) {
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
  if (check.class) {
    class(xgrid$x1) <- class(newdata[[x1.name]])
    class(xgrid$x2) <- class(newdata[[x2.name]])
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
