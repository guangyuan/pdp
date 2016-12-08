# TODO:
#   * Allow column position specification in pred.grid.
#   * Update documentation to give details as to how the data is extracted from
#     object when train is not specified.


#' Partial Dependence Functions
#'
#' Compute partial dependence functions (i.e., marginal effects) for various
#' model fitting objects.
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
#'   Current options are \code{"auto"}, \code{"regression"} or
#'   \code{"classification"}. If \code{type = "auto"} then \code{partial} try
#'   to extract the necessary information from \code{object}.
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
#'   training values; this affects \code{pred.grid}. Default is \code{FALSE}.
#' @param train An optional data frame containing the original training
#'   data. This may be required depending on the class of \code{object}. For
#'   objects that do not store a copy of the original training data, this
#'   argument is required.
#' @param check.class Logical indicating whether or not to make sure each column
#'   in \code{pred.grid} has the correct class, levels, etc. Default is
#'   \code{TRUE}.
#' @param progress Character string giving the name of the progress bar to use.
#'   See \code{plyr::create_progress_bar} for details. Default is \code{"none"}.
#' @param parallel Logical indicating whether or not to run \code{partial} in
#'   parallel using a backend provided by the \code{foreach} package. Default is
#'   \code{FALSE}. Default is \code{NULL}.
#' @param paropts List containing additional options passed onto
#'   \code{foreach::foreach} when \code{parallel = TRUE}.
#' @param ... Additional optional arguments to be passed onto
#'   \code{stats::predict}.
#'
#' @references
#' J. H. Friedman. Greedy function approximation: A gradient boosting machine.
#' \emph{Annals of Statistics}, \bold{29}: 1189-1232, 2000.
#'
#' @rdname partial
#' @export
#' @examples
#' \dontrun{
#' #
#' # Regression example (requires randomForest package to run)
#' #
#'
#' # Fit a random forest to the boston housing data
#' library(randomForest)
#' data (boston)  # load the boston housing data
#' set.seed(101)  # for reproducibility
#' boston.rf <- randomForest(cmedv ~ ., data = boston)
#'
#' # Using randomForest's partialPlot function
#' partialPlot(boston.rf, pred.data = boston, x.var = "lstat")
#'
#' # Using pdp's partial function
#' head(partial(boston.rf, pred.var = "lstat"))  # returns a data frame
#' partial(boston.rf, pred.var = "lstat", plot = TRUE, rug = TRUE)
#'
#' # The partial function allows for multiple predictors
#' partial(boston.rf, pred.var = c("lstat", "rm"), grid.resolution = 40,
#'         plot = TRUE, chull = TRUE, .progress = "text")
#'
#' # The plotPartial function offers more flexible plotting
#' pd <- partial(boston.rf, pred.var = c("lstat", "rm"), grid.resolution = 40)
#' plotPartial(pd)  # the default
#' plotPartial(pd, levelplot = FALSE, zlab = "cmedv", drape = TRUE,
#'             colorkey = FALSE, screen = list(z = -20, x = -60))
#'
#' #
#' # Classification example (requires randomForest package to run)
#' #
#'
#' # Fit a random forest to the Pima Indians diabetes data
#' data (pima)  # load the boston housing data
#' set.seed(102)  # for reproducibility
#' pima.rf <- randomForest(diabetes ~ ., data = pima, na.action = na.omit)
#'
#' # Partial dependence of glucose on diabetes test result (neg/pos)
#' partial(pima.rf, pred.var = c("glucose", "age"), plot = TRUE, chull = TRUE,
#'         .progress = "text")
#'
#' #
#' # Interface with caret (requires caret package to run)
#' #
#'
#' # Load required packages
#' library(caret)  # for model training/tuning
#'
#' # Set up for 5-fold cross-validation
#' ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
#'
#' # Tune a support vector machine (SVM) using a radial basis function kerel to
#' # the Pima Indians diabetes data
#' set.seed(103)  # for reproducibility
#' pima.svm <- train(diabetes ~ ., data = pima, method = "svmRadial",
#'                   prob.model = TRUE, na.action = na.omit, trControl = ctrl,
#'                   tuneLength = 10)
#'
#' # Partial dependence of glucose on diabetes test result (neg/pos)
#' partial(pima.svm, pred.var = "glucose", plot = TRUE, rug = TRUE)
#' }
partial <- function(object, ...) {
  UseMethod("partial")
}


#' @rdname partial
#' @export
partial.default <- function(object, pred.var, pred.grid, grid.resolution = NULL,
                            type = c("auto", "regression", "classification"),
                            which.class = 1L, plot = FALSE,
                            smooth = FALSE, rug = FALSE, chull = FALSE, train,
                            check.class = TRUE, progress = "none",
                            parallel = FALSE, paropts = NULL, ...) {

  # Data frame
  if (missing(train)) {
    if (inherits(object, "BinaryTree") || inherits(object, "RandomForest")) {
      train <- object@data@get("input")
    } else if (inherits(object, "train")) {
      train <- object$trainingData
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

  # Throw error message if predictor names not found in training data
  if (!all(pred.var %in% names(train))) {
    stop(paste(paste(pred.var[!(pred.var %in% names(train))], collapse = ", "),
               "not found in the training data."))
  }

  # NOTE: It is worth considering the approach taken by the plotmo package,
  # which now has the option to compute PDPs. plotmo seems to make one large
  # pred.grid which increases memory usage, but decreases the number of calls to
  # stats::predict.

  # Predictor values of interest
  if (missing(pred.grid)) {
    pred.grid <- predGrid(object, pred.var = pred.var, train = train,
                          grid.resolution = grid.resolution)
  }

  # Make sure each column has the correct class, factor levels, etc.
  if (check.class) {
    pred.grid <- copyClasses(pred.grid, train)
  }

  # Restrict grid to covext hull of first two columns
  if (chull) {
    pred.grid <- trainCHull(pred.var, pred.grid = pred.grid, train = train)
  }

  # Determine the type of supervised learning used
  type <- match.arg(type)
  if (type == "auto") {
    type <- superType(object)
  }

  # Calculate partial dependence values
  if (type == "regression") {
    pd.df <- pdRegression(object, pred.var = pred.var, pred.grid = pred.grid,
                          train = train, progress = progress,
                          parallel = parallel, paropts = paropts, ...)
  } else if (type == "classification") {
    pd.df <- pdClassification(object, pred.var = pred.var,
                              pred.grid = pred.grid, which.class = which.class,
                              train = train, progress = progress,
                              parallel = parallel, paropts = paropts, ...)
  } else {
    stop(paste("Partial dependence values are currently only available",
               "for classification and regression problems."))
  }

  # Create data frame of partial dependence values
  names(pd.df) <- c(pred.var, "y")
  class(pd.df) <- c("data.frame", "partial")

  # Plot partial dependence function (if requested)
  if (plot) {
    res <- plotPartial(pd.df, smooth = smooth, rug = rug, train = train,
                       col.regions = viridis::viridis)
    attr(res, "partial.data") <- pd.df
  } else {
    # Return partial dependence values
    res <- pd.df
  }
  
  # Return results
  res

}
