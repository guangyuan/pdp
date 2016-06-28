#' Partial Dependence Functions
#'
#' Compute partial dependence functions for various model fitting objects.
#'
#' @param object A fitted model object.
#' @param pred.var Character string giving the names of the independent
#'   variables of interest.
#' @param pred.grid Data frame containing the joint values of the variables
#'   listed in \code{pred.var}.
#' @param grid.resolution Integer giving the number of equally spaced points to
#'   use (only used for the continuous variables listed in \code{pred.var} when
#'   \code{pred.grid} is not supplied). If left \code{NULL}, it will default to
#'   minimum between \code{51} and the number of unique data points for each of
#'   the continuous independent variables listed in \code{pred.var}.
#' @param super.type Character string specifying the type of supervised learning.
#'   Current options are \code{"regression"} or \code{"classification"}. For tree-
#'   based models (e.g., \code{"rpart"}), the function can usually extract the
#'   necessary information from \code{object}.
#' @param which.class Integer specifying which column of the matrix of predicted
#'   probabilities to use as the "focus" class. Default is to use the first class.
#' @param check.class Logical indicating whether or not to check the class of
#'   the predictor variable of interest. Default is \code{TRUE}.
#' @param training.data An optional data frame containing the original training
#'   data.
#' @param plot Logical indicating whether to return a data frame containing the
#'   partial dependence values (\code{FALSE}) or plot the partial dependence
#'   function directly (\code{TRUE}). Default is \code{FALSE}.
#' @param ... Additional optional arguments to be passed onto \code{aaply}.
#'
#' @rdname partial
#' @importFrom plyr adply laply
#' @importFrom stats predict
#' @export
partial <- function(object, ...) {
  UseMethod("partial")
}


#' @rdname partial
#' @export
partial.default <- function(object, pred.var, pred.grid, grid.resolution = NULL,
                            super.type, which.class = 1L, check.class = TRUE,
                            training.data, plot = FALSE, ...) {

  # Data frame
  if (missing(training.data)) {
    if (inherits(object, "BinaryTree") || inherits(object, "RandomForest")) {
      training.data <- object@data@get("input")
    } else {
      if (is.null(object$call$data)) {
        stop("No data found.")
      } else {
        training.data <- eval(object$call$data)
      }
    }
  }

  # Predictor values of interest
  if (missing(pred.grid)) {
    pred.val <- lapply(pred.var, function(x) {
      if (is.factor(training.data[[x]])) {
        levels(training.data[[x]])
      #} #else if (missing(grid.resolution)) {
        #sort(unique(newdata[[x]]))
      } else {
        if (is.null(grid.resolution)) {
          grid.resolution <- min(length(unique(training.data[[x]])), 51)
        }
        seq(from = min(training.data[[x]], na.rm = TRUE),
            to = max(training.data[[x]], na.rm = TRUE),
            length = grid.resolution)
      }
    })
    pred.grid <- expand.grid(pred.val)
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
    # pd_df <- adply(pred.grid, .margins = 1, .fun = function(x) {
    #   temp <- training.data
    #   temp[pred.var] <- x
    #   mean(predict(object, newdata = temp), na.rm = TRUE)
    # }, ...)
    pd_df <- pdRegression(object, pred.var = pred.var, pred.grid = pred.grid,
                          training.data = training.data, ...)
  } else if (super.type == "classification") {
    # pd_df <- adply(pred.grid, .margins = 1, .fun = function(x) {
    #   temp <- training.data
    #   temp[pred.var] <- x
    #   pr <- predict(object, newdata = temp, type = "prob")
    #   avgLogit(pr, which.class = which.class)
    # }, ...)
    pd_df <- pdClassification(object, pred.var = pred.var,
                              pred.grid = pred.grid, which.class = which.class,
                              training.data = training.data, ...)
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
