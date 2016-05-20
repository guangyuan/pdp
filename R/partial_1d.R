
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
#' @param super.type Character string specifying the type of supervised
#'   learning. Current options are \code{"regression"} or 
#'   \code{"classification"}. For tree-based models (e.g., \code{"rpart"}), the
#'   function can usually extract the necessary information from \code{object}.
#' @param check.class Logical indicating whether or not to check the class of
#'   the predictor variables of interest.
#' @param ... Additional optional arguments passed onto \code{aaply}.
#' @note
#' It may be necessary to supply a value for \code{x.class} when \code{n} is
#' supplied. This is usually the case when \code{object} is of class
#' \code{"BinaryTree"} or \code{"mertree"} (with \code{unbiased = TRUE}).
#' @importFrom plyr laply
#' @rdname partial_1d
#' @export
partial_1d <- function(object, x.name, x.values, n, newdata, which.class = 1L,
                       super.type, ...) {
  UseMethod("partial_1d")
}


#' @rdname partial_1d
#' @export
partial_1d.default <- function(object, x.name, x.values, n, newdata,
                               which.class = 1L, check.class = TRUE, ...) {
  
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
  
  # Determine the type of supervised learning used
  if (missing(super.type)) {
    super.type <- superType(object)
  } else {
    if (!(super.type %in% c("regression", "classification"))) {
      stop("Only regression and classification are supported.")
    }
  }
  
  # Make sure x.values has the correct x.class
  if (check.class) {
    class(x.values) <- x.class
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
  pd_df
  
}
