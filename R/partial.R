#' Partial Dependence Functions
#'
#' Compute partial dependence functions for various model fitting objects.
#'
#' @param object A fitted model object of appropriate class (e.g.,
#'   \code{"gbm"}, \code{"lm"}, \code{"randomForest"}, etc.).
#' @param pred.var Character string giving the names of the predictor
#'   variables of interest. For reasons of computation/interpretation, this
#'   should include no more than three variables.
#' @param pred.grid Data frame containing the joint values of the variables
#'   listed in \code{pred.var}.
#' @param grid.resolution Integer giving the number of equally spaced points to
#'   use (only used for the continuous variables listed in \code{pred.var} when
#'   \code{pred.grid} is not supplied). If left \code{NULL}, it will default to
#'   the minimum between \code{51} and the number of unique data points for each
#'   of the continuous independent variables listed in \code{pred.var}.
#' @param type Character string specifying the type of supervised learning.
#'   Current options are \code{"regression"} or \code{"classification"}. For
#'   some objects (e.g., tree-based models like \code{"rpart"}), \code{partial}
#'   can usually extract the necessary information from \code{object}.
#' @param which.class Integer specifying which column of the matrix of predicted
#'   probabilities to use as the "focus" class. Default is to use the first
#'   class. Only used for classification problems (i.e., when
#'   \code{type = "classification"}).
#' @param plot Logical indicating whether to return a data frame containing the
#'   partial dependence values (\code{FALSE}) or plot the partial dependence
#'   function directly (\code{TRUE}). Default is \code{FALSE}.
#' @param smooth Logical indicating whether or not to overlay a LOESS smooth.
#'   Default is \code{FALSE}.
#' @param rug Logical indicating whether or not to include rug marks on the
#'   predictor axes. Only used when \code{plot = TRUE}. Default is \code{FALSE}.
#' @param chull Logical indicating wether or not to restrict the first
#'   two variables in \code{pred.var} to lie within the convex hull of their
#'   data points; this effects \code{pred.grid}. Default is \code{FALSE}.
#' @param train An optional data frame containing the original training
#'   data. This may be required depending on the class of \code{object}. For
#'   objects that do not store a copy of the original training data, this
#'   argument is required.
#' @param check.class Logical indicating whether or not to make sure each column
#'   in \code{pred.grid} has the correct class, levels, etc. Default is
#'   \code{TRUE}.
#' @param ... Additional optional arguments to be passed onto
#'   \code{plyr::aaply}.
#'
#' @references
#' J. H. Friedman. Greedy function approximation: A gradient boosting machine.
#' \emph{Annals of Statistics}, \bold{29}: 1189-1232, 2000.
#'
#' @rdname partial
#' @export
#' @examples
#' # Fit a random forest to the airquality data
#' library(randomForest)
#' data(airquality)
#' set.seed(101)  # for reproducibility
#' ozone.rf <- randomForest(Ozone ~ ., data = airquality, importance = TRUE,
#'                          na.action = na.omit)
#'
#' # Using randomForest's partialPlot function
#' partialPlot(ozone.rf, pred.data = airquality, x.var = "Temp")
#'
#' # Using pdp's partial function
#' head(partial(ozone.rf, pred.var = "Temp"))  # returns a data frame
#' partial(ozone.rf, pred.var = "Temp", plot = TRUE, rug = TRUE)
#'
#' # The partial function allows for multiple predictors
#' partial(ozone.rf, pred.var = c("Temp", "Wind"), grid.resolution = 20,
#'         plot = TRUE, chull = TRUE, .progress = "text")
#'
#' # The plotPartial function offers more flexible plotting
#' pd <- partial(ozone.rf, pred.var = c("Temp", "Wind"), grid.resolution = 20,
#'               chull = TRUE)
#' plotPartial(pd)  # the default
#' plotPartial(pd, levelplot = FALSE, zlab = "Ozone", drape = TRUE,
#'             colorkey = FALSE, screen = list(z = 120, x = -60))
partial <- function(object, ...) {
  UseMethod("partial")
}


#' @rdname partial
#' @export
partial.default <- function(object, pred.var, pred.grid, grid.resolution = NULL,
                            type, which.class = 1L, plot = FALSE,
                            smooth = FALSE, rug = FALSE, chull = FALSE, train,
                            check.class = TRUE, ...) {

  # Data frame
  if (missing(train)) {
    if (inherits(object, "BinaryTree") || inherits(object, "RandomForest")) {
      train <- object@data@get("input")
    } else {
      if (is.null(object$call$data)) {
        stop(paste0("The training data could not be extracted from ",
                    deparse(substitute(object)), ". Please supply the raw ",
                    "training data using the `train` argument in the call ",
                    "to `partial`."))
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
    names(pred.grid) <- pred.var
  }

  # Make sure each column has the correct class, levels, etc.
  if (check.class) {
    pred.grid <- copyClasses(pred.grid, train)
  }

  # Restrict grid to covext hull of first two columns
  if (chull) {
    if (length(pred.var) >= 2 && is.numeric(train[[1L]]) &&
        is.numeric(train[[2L]])) {
      X <- stats::na.omit(data.matrix(train[pred.var[1L:2L]]))
      Y <- stats::na.omit(data.matrix(pred.grid[1L:2L]))
      hpts <- grDevices::chull(X)
      hpts <- c(hpts, hpts[1])
      keep <- mgcv::in.out(X[hpts, ], Y)
      pred.grid <- pred.grid[keep, ]
    }
  }

  # Determine the type of supervised learning used
  if (missing(type)) {
    type <- superType(object)
  } else {
    if (!(type %in% c("regression", "classification"))) {
      stop(paste(deparse(substitute(type)), 'is not a valid value for `type`.',
                 'Please select either "regression" or "classification".'))
    }
  }

  # Calculate partial dependence values
  if (type == "regression") {
    pd_df <- pdRegression(object, pred.var = pred.var, pred.grid = pred.grid,
                          train = train, ...)
  } else if (type == "classification") {
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
    print(plotPartial(pd_df, smooth = smooth, rug = rug, train = train,
                      col.regions = viridis::viridis))
  } else {
    # Return partial dependence values
    pd_df
  }

}
