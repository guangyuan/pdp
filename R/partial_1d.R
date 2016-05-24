
#' Partial Dependence (One Variable)
#'
#' One- and two-dimensional partial dependence plots for objects of class
#' \code{mertree}.
#'
#' @param object A \code{mertree} object.
#' @param x.name Character string giving the name of the independent variable of
#'   interest.
#' @param x.values Vector of predictor values to use in computing the partial 
#'   dependence function.
#' @param n Integer giving the number of unique data points to use in
#'   computing the partial dependence values.
#' @param newdata An optional data frame.
#' @param which.class Integer specifying which column of the matrix of predicted 
#'   probabilities to use as the "focus" class. Default is to use the first class.
#' @param super.type Character string specifying the type of supervised
#'   learning. Current options are \code{"regression"} or 
#'   \code{"classification"}. For tree-based models (e.g., \code{"rpart"}), the
#'   function can usually extract the necessary information from \code{object}.
#' @param check.class Logical indicating whether or not to check the class of
#'   the predictor variable of interest. Default is \code{TRUE}.
#' @param ... Additional optional arguments to be passed onto \code{aaply}.
#'
#' @importFrom plyr laply
#' @rdname partial_1d
#' @export
partial_1d <- function(object, ...) {
  UseMethod("partial_1d")
}


#' @rdname partial_1d
#' @export
partial_1d.default <- function(object, x.name, x.values, n, newdata,
                               which.class = 1L, super.type, check.class = TRUE, 
                               ...) {
  
  # Data frame
  newdata <- if (missing(newdata)) eval(object$call$data) else newdata
  
  # Predictor values of interest
  x.values <- if (missing(x.values)) {
    if (is.factor(newdata[[x.name]])) {
      levels(newdata[[x.name]])
    } else if (missing(n)) {
      sort(unique(newdata[[x.name]]))
    } else {
      seq(from = min(newdata[[x.name]]), to = max(newdata[[x.name]]), length = n)
    }
  }
  
  # Make sure x.values has the correct x.class
  if (check.class) {
    class(x.values) <- class(newdata[[x.name]])
  }
  
  # Determine the type of supervised learning used
  if (missing(super.type)) {
    super.type <- superType(object)
  } else {
    if (!(super.type %in% c("regression", "classification"))) {
      stop("Only regression and classification are supported.")
    }
  }
  
  # Calculate partial dependence values
  if (super.type == "regression") {
    pd <- laply(x.values, .fun = function(x) {
      temp <- newdata
      temp[[x.name]] <- x
      mean(predict(object, newdata = temp), na.rm = TRUE)
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
  class(pd_df) <- c("data.frame", "partial_1d")
  pd_df
  
}
