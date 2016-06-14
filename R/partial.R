#' Partial Dependence Functions
#'
#' Compute partial dependence functions for various model fitting objects.
#'
#' @param object A fitted model object.
#' @param pred.var Character string giving the names of the independent
#'   variables of interest.
#' @param n.pts Integer giving the number of unique data points to use in
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
#' @param plot Logical indicating whether to return a data frame containing the
#'   partial dependence values (\code{FALSE}) or plot the partial dependence
#'   function directly (\code{TRUE}). Default is \code{FALSE}.
#' @param ... Additional optional arguments to be passed onto \code{aaply}.
#'
#' @rdname partial
#' @importFrom plyr laply
#' @export
partial <- function(object, ...) {
  UseMethod("partial")
}


#' @rdname partial
#' @export
partial.default <- function(object, pred.var, n.pts = NULL, super.type,
                            which.class = 1L, check.class = TRUE,
                            newdata, plot = FALSE, ...) {

  # Data frame
  if (missing(newdata)) {
    if (inherits(object, "BinaryTree") || inherits(object, "RandomForest")) {
      newdata <- object@data@get("input")
    } else {
      if (is.null(object$call$data)) {
        stop("No data found.")
      } else {
        newdata <- eval(object$call$data)
      }
    }
  }

  # Predictor variable classes
  classes <- sapply(pred.var, function(x) {
    class(newdata[[x]])
  })

  # Predictor values of interest
  pred.val <- lapply(pred.var, function(x) {
    if (is.factor(newdata[[x]])) {
      levels(newdata[[x]])
    } else if (is.null(n.pts)) {
      sort(unique(newdata[[x]]))
    } else {
      seq(from = min(newdata[[x]], na.rm = TRUE),
          to = max(newdata[[x]], na.rm = TRUE), length = n.pts)
    }
  })
  pred.grid <- expand.grid(pred.val)

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
    pd_df <- adply(pred.grid, .margins = 1, .fun = function(x) {
      temp <- newdata
      temp[pred.var] <- x
      mean(predict(object, newdata = temp), na.rm = TRUE)
    }, ...)
  } else if (super.type == "classification") {
    pd_df <- adply(pred.grid, .margins = 1, .fun = function(x) {
      temp <- newdata
      temp[pred.var] <- x
      pr <- predict(object, newdata = temp, type = "prob")
      avgLogit(pr, which.class = which.class)
    }, ...)
  } else {
    stop(paste("Partial dependence values are currently only available",
               "for classification and regression problems."))
  }

  # Create data frame of partial dependence values
  names(pd_df) <- c(pred.var, "y")
  class(pd_df) <- c("data.frame", "partial")

  # Plot partial dependence function (if requested)
  if (plot) {
    print(plotPartial(pd_df))
  } else {
    # Return partial dependence values
    pd_df
  }

}
