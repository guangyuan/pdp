#' Extract Most "Important" Predictors (Experimental)
#'
#' Extract the most "important" predictors for regression and classification
#' models.
#'
#' @param object A fitted model object of appropriate class (e.g.,
#'   \code{"gbm"}, \code{"lm"}, \code{"randomForest"}, etc.).
#' @param n Integer specifying the number of predictors to return. Default is
#'   \code{1} meaning return the single most important predictor.
#' @param ... Additional optional arguments to be passed onto
#'   \code{\link[caret]{varImp}}.
#'
#' @details
#' This function uses the generic function \code{\link[caret]{varImp}} to
#' calculate variable importance scores for each predictor. After that, they are
#' sorted at the names of the \code{n} highest scoring predictors are returned.
#'
#' @rdname topPredictors
#' @export
#' @examples
#' \dontrun{
#'
#' # Fit a random forest to the boston housing data
#' library(randomForest)
#' data (boston)  # load the boston housing data
#' set.seed(101)  # for reproducibility
#' boston.rf <- randomForest(cmedv ~ ., data = boston)
#'
#' # What are the top four predictors?
#' topPredictors(boston.rf, n = 4)
#'
#' # Construct PDPs for the top four predictors
#' par(mfrow = c(2, 2))
#' for (pred in topPredictors(boston.rf, 4)) {
#'   plot(partial(boston.rf, pred.var = pred), type = "l")
#' }
#'
#' # Construct a two-way PDP for the top two predictors
#' boston.rf %>%
#'   partial(topPredictors(., n = 2), chull = TRUE) %>%
#'   plotPartial()
#'
#' }
topPredictors <- function(object, n = 1L, ...) {
  UseMethod("topPredictors")
}


#' @rdname topPredictors
#' @export
topPredictors.default <- function(object, n = 1L, ...) {
  imp <- caret::varImp(object)
  if (n > nrow(imp)) {
    n <- nrow(imp)
  }
  imp <- imp[order(imp[, "Overall"], decreasing = TRUE), , drop = FALSE]
  rownames(imp)[seq_len(n)]
}


#' @rdname topPredictors
#' @export
topPredictors.train <- function(object, n = 1L, ...) {
  imp <- caret::varImp(object)$importance
  if (n > nrow(imp)) {
    n <- nrow(imp)
  }
  imp <- imp[order(imp$Overall, decreasing = TRUE), , drop = FALSE]
  rownames(imp)[seq_len(n)]
}
