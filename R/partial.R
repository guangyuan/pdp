#' Partial Dependence Functions
#'
#' Compute partial dependence functions for various model fitting objects.
#'
#' @param object A fitted model object.
#' @param pred.var Character string giving the names of the predictor
#'   variables of interest.
#' @param pred.grid Data frame containing the joint values of the variables
#'   listed in \code{pred.var}.
#' @param grid.resolution Integer giving the number of equally spaced points to
#'   use (only used for the continuous variables listed in \code{pred.var} when
#'   \code{pred.grid} is not supplied). If left \code{NULL}, it will default to
#'   minimum between \code{51} and the number of unique data points for each of
#'   the continuous independent variables listed in \code{pred.var}.
#' @param super.type Character string specifying the type of supervised learning.
#'   Current options are \code{"regression"} or \code{"classification"}. For some
#'   objects (e.g., tree-based models like \code{"rpart"}), \code{partial} can usually 
#'   extract the necessary information from \code{object}.
#' @param which.class Integer specifying which column of the matrix of predicted
#'   probabilities to use as the "focus" class. Default is to use the first class.
#' @param plot Logical indicating whether to return a data frame containing the
#'   partial dependence values (\code{FALSE}) or plot the partial dependence
#'   function directly (\code{TRUE}). Default is \code{FALSE}.
#' @param rug Logical indicating whether or not to include a rug representation
#'   to the plot. Only used when \code{plot = TRUE}. Default is \code{FALSE}.
#' @param chull Logical indicating wether or not to restrict the first
#'   two variables in \code{pred.var} to lie within the convex hull of their
#'   data points; this effects \code{pred.grid}. Default is \code{FALSE}.
#' @param train An optional data frame containing the original training
#'   data. This may be required depending on the class of \code{object}.
#' @param ... Additional optional arguments to be passed onto \code{aaply}.
#'
#' @rdname partial
#' @export
partial <- function(object, ...) {
  UseMethod("partial")
}


#' @rdname partial
#' @export
partial.default <- function(object, pred.var, pred.grid, grid.resolution = NULL,
                            super.type, which.class = 1L, plot = TRUE, 
                            rug = FALSE, chull = FALSE, train, ...) {

  # Data frame
  if (missing(train)) {
    if (inherits(object, "BinaryTree") || inherits(object, "RandomForest")) {
      train <- object@data@get("input")
    } else {
      if (is.null(object$call$data)) {
        stop("No data found.")
      } else {
        train <- eval(object$call$data)
      }
    }
  }

  # Predictor values of interest
  if (missing(pred.grid)) {
    pred.val <- lapply(pred.var, function(x) {
      if (is.factor(train[[x]])) {
        levels(train[[x]])
      #} #else if (missing(grid.resolution)) {
        #sort(unique(newdata[[x]]))
      } else {
        if (is.null(grid.resolution)) {
          grid.resolution <- min(length(unique(train[[x]])), 51)
        }
        seq(from = min(train[[x]], na.rm = TRUE),
            to = max(train[[x]], na.rm = TRUE),
            length = grid.resolution)
      }
    })
    pred.grid <- expand.grid(pred.val)
  }
  names(pred.grid) <- pred.var  # FIXME: Is this even needed here?

  # Restrict grid to covext hull of first two columns
  if (chull) {
    if (length(pred.var) >= 2 && is.numeric(train[[1L]]) &&
        is.numeric(train[[2L]])) {
      X <- data.matrix(train[pred.var[1L:2L]])
      Y <- data.matrix(pred.grid[1L:2L])
      hpts <- grDevices::chull(X)
      hpts <- c(hpts, hpts[1])
      keep <- mgcv::in.out(X[hpts, ], Y)
      pred.grid <- pred.grid[keep, ]
    }
  }

  # # Restore class information
  # for (name in names(pred.grid)) {
  #   if (is.integer(train[[name]])) {
  #     pred.grid[[pred.var]] <- as.intergar(pred.grid[[pred.var]])
  #   } else if (is.numeric(train[[name]])) {
  #     pred.grid[[pred.var]] <- as.numeric(pred.grid[[pred.var]])
  #
  #   } else {
  #     class(pred.grid[[name]]) <- class(train[[name]])
  #   }
  # }
  #
  # # Sanity check!
  # stopifnot(all.equal(sapply(pred.grid, class),
  #                     sapply(train[pred.var], class)))

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
    pd_df <- pdRegression(object, pred.var = pred.var, pred.grid = pred.grid,
                          train = train, ...)
  } else if (super.type == "classification") {
    pd_df <- pdClassification(object, pred.var = pred.var,
                              pred.grid = pred.grid, which.class = which.class,
                              train = train, ...)
  } else {
    stop(paste("Partial dependence values are currently only available",
               "for classification and regression problems."))
  }

  # Create data frame of partial dependence values
  names(pd_df) <- c(pred.var, "y")
  class(pd_df) <- c("data.frame", "partial")

  # Plot partial dependence function (if requested)
  if (plot) {
    print(plotPartial(pd_df, rug = rug, train = train, col.regions = viridis::viridis))
  } else {
    # Return partial dependence values
    pd_df
  }

}
