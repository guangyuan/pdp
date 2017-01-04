#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Arrange multiple grobs on a page
#'
#' See \code{\link[gridExtra]{grid.arrange}} for more details.
#'
#' @name grid.arrange
#' @rdname grid.arrange
#' @keywords internal
#' @export
#' @importFrom gridExtra grid.arrange
#' @usage grid.arrange(..., newpage = TRUE)
NULL


#' Retrieve the last Trellis Object
#'
#' See \code{\link[lattice]{trellis.last.object}} for more details.
#'
#' @name trellis.last.object
#' @rdname trellis.last.object
#' @keywords internal
#' @export
#' @importFrom lattice  trellis.last.object
#' @usage trellis.last.object(..., prefix)
NULL


#' @keywords internal
copyClasses <- function(x, y) {
  x.names <- names(x)
  y.names <- names(y)
  if (length(setdiff(x.names, y.names)) > 0) {
    stop("Data frame x contains columns not present in data frame y.")
  }
  column.names <- intersect(x.names, y.names)
  for (name in column.names) {
    # Do the classes match?
    if (!identical(class(x[[name]]), class(y[[name]])) ||
        !identical(levels(x[[name]]), levels(y[[name]]))) {
      # Convert to numeric or integer class
      if (is.numeric(y[[name]])) {
        if (is.integer(y[[name]])) {
          x[[name]] <- as.integer(x[[name]])
        } else {
          x[[name]] <- as.numeric(x[[name]])
        }
      }
      # Convert to factor or ordered class
      if (is.factor(y[[name]])) {
        if (is.ordered(y[[name]])) {
          x[[name]] <- as.ordered(x[[name]])
        } else {
          x[[name]] <- as.factor(x[[name]])
        }
        levels(x[[name]]) <- levels(y[[name]])
      }
      # Convert to character
      if (is.character(y[[name]])) {
        x[[name]] <- as.character(x[[name]])
      }
      # Convert to logical
      if (is.logical(y[[name]])) {
        x[[name]] <- as.logical(x[[name]])
      }
    }
  }
  # Sanity check
  stopifnot(all.equal(sapply(x[column.names], class),
                      sapply(y[column.names], class)))
  x  # return x with copied classes
}


#' @keywords internal
avgLogit <- function(x, which.class = 1L) {
  if (is.data.frame(x)) {
    x <- data.matrix(x)
  }
  stopifnot(is.matrix(x))  # x should be a nclass by n probability matrix
  eps <- .Machine$double.eps
  mean(log(ifelse(x[, which.class] > 0, x[, which.class], eps)) -
         rowMeans(log(ifelse(x > 0, x, eps))), na.rm = TRUE)
}


#' @keywords internal
trainCHull <- function(pred.var, pred.grid, train) {
  if (length(pred.var) >= 2 && is.numeric(pred.grid[[1L]]) &&
      is.numeric(pred.grid[[2L]])) {
    X <- stats::na.omit(data.matrix(train[pred.var[1L:2L]]))
    Y <- stats::na.omit(data.matrix(pred.grid[1L:2L]))
    hpts <- grDevices::chull(X)
    hpts <- c(hpts, hpts[1L])
    keep <- mgcv::in.out(X[hpts, ], Y)
    pred.grid[keep, ]
  } else {
    pred.grid
  }
}


# Error message to display when training data cannot be extracted form object
mssg <- paste0("The training data could not be extracted from ",
               deparse(substitute(object)), ". Please supply the raw ",
               "training data using the `train` argument in the call to ",
               "`partial`.")


#' @keywords internal
getTrainingData <- function(object) {
  UseMethod("getTrainingData")
}


#' @keywords internal
getTrainingData.default <- function(object) {
  if (isS4(object)) {
    stop(mssg)
  } else {
    train <- eval(stats::getCall(object)$data)
    if (is.null(train)) {
      stop(mssg)
    } else {
      # warning(paste0("No training data given! Attempting to extract training",
      #                " data from ", deparse(substitute(object)), "'s call. ",
      #                "If available, the data will be evaluated within the ",
      #                "environment from which `partial` was called."),
      #         call. = FALSE)
      if (!(is.data.frame(train))) {
        if (is.matrix(train) || is.list(train)) {
          train <- as.data.frame(train)
        } else {
          stop(mssg)
        }
      }
    }
  }
  train
}


#' @keywords internal
getTrainingData.BinaryTree <- function(object) {
  object@data@get("input")
}


#' @keywords internal
getTrainingData.RandomForest <- function(object) {
  object@data@get("input")
}


#' @keywords internal
getTrainingData.train <- function(object) {
  # By default, "train" object have a copy of the training data stored in
  # a components called "trainingData"
  train <- object$trainingData
  if (is.null(train)) {
    stop(mssg)
  }
  train$.outcome <- NULL  # remove .outcome column
  train
}


#' @keywords internal
predGrid <- function(object, pred.var, train, grid.resolution = NULL) {
  UseMethod("predGrid")
}


#' @keywords internal
predGrid.default <- function(object, pred.var, train, grid.resolution = NULL) {
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
  pred.grid
}


# TODO (bgreenwell):
# predGrid.rpart <- NULL
# predGrid.BinaryTree <- NULL
# predGrid.ctree <- NULL
# predGrid.randomForest <- NULL
# predGrid.RandomForest <- NULL


#' @keywords internal
superType <- function(object) {
  UseMethod("superType")
}


#' @keywords internal
superType.default <- function(object) {
  warning('`type` could not be determined; assuming `type = "regression"`')
  "regression"
}


#' @keywords internal
superType.train <- function(object) {
  if (object$modelType == "Classification") {
    "classification"
  } else if (object$modelType == "Regression") {
    "regression"
  } else {
    "other"
  }
}


#' @keywords internal
superType.bagging <- function(object) {
  "classification"
}


#' @keywords internal
superType.boosting <- function(object) {
  "classification"
}


#' @keywords internal
superType.classbagg <- function(object) {
  "classification"
}


#' @keywords internal
superType.regbagg <- function(object) {
  "regression"
}


#' @keywords internal
superType.cubist <- function(object) {
  "regression"
}


#' @keywords internal
superType.C5.0 <- function(object) {
  "classification"
}


#' @keywords internal
superType.earth <- function(object) {
  if (!is.null(object$glm.list) &&
      object$glm.list[[1L]]$family$family == "binomial") {
    "classification"
  } else if (is.null(object$glm.list) ||
             object$glm.list[[1L]]$family$family == "gaussian") {
    "regression"
  } else {
    "other"
  }
}


#' @keywords internal
superType.lm <- function(object) {
  # FIXME: What about multivariate response models?
  "regression"
}


#' @keywords internal
superType.nls <- function(object) {
  "regression"
}


#' @keywords internal
superType.ppr <- function(object) {
  if (object$q > 1) {
    stop("`partial` does not currently support multivariate response models.")
  }
  "regression"
}


#' @keywords internal
superType.glm <- function(object) {
  if(object$family$family == "binomial") {
    "classification"
  } else if (object$family$family %in%
             c("gaussian", "Gamma", "inverse.gaussian", "poisson")) {
    "regression"
  } else {
    "other"
  }
}


#' @keywords internal
superType.multinom <- function(object) {
  # FIXME: What about multivariate response models?
  "classification"
}


#' @keywords internal
superType.rpart <- function(object) {
  if (object$method == "anova") {
    "regression"
  } else if (object$method == "class") {
    "classification"
  } else {
    "other"
  }
}


#' @keywords internal
superType.BinaryTree <- function(object) {
  if (object@responses@is_nominal) {
    "classification"
  } else if (object@responses@is_ordinal || object@responses@is_censored) {
    "other"
  } else {
    "regression"
  }
}


#' @keywords internal
superType.randomForest <- function(object) {
  if (object$type == "regression") {
    "regression"
  } else if (object$type == "classification") {
    "classification"
  } else {
    "unsupervised"
  }
}


#' @keywords internal
superType.ranger <- function(object) {
  if (object$treetype == "Regression") {
    "regression"
  } else if (object$treetype == "Probability estimation") {
    "classification"
  } else {
    "other"
  }
}


#' @keywords internal
superType.RandomForest <- function(object) {
  if (object@responses@is_nominal) {
    "classification"
  } else if (object@responses@is_ordinal || object@responses@is_censored) {
    "other"
  } else {
    "regression"
  }
}


#' @keywords internal
superType.gbm <- function(object) {
  if (object$distribution %in% c("gaussian", "laplace", "tdist")) {
    "regression"
  } else if (object$distribution %in% c("bernoulli", "huberized", "multinomial",
                                        "adaboost")) {
    "classification"
  } else {
    "other"
  }
}


#' @keywords internal
superType.xgb.Booster <- function(object) {
  if (object$params$objective == "reg:linear") {
    "regression"
  } else if (object$params$objective %in%
             c("binary:logistic", "multi:softprob")) {
    # FIXME: Throw a warning if objective function is classification, but does
    # not return the predicted probabilities (e.g., "binary:logitraw").
    "classification"
  } else if (object$params$objective %in%
             c("reg:logistic", "binary:logitraw", "multi:softmax")) {
    stop(paste("For classification, switch to an objective function",
               "that returns the predicted probabilities."))
  } else {
    "other"
  }
}


#' @keywords internal
superType.svm <- function(object) {
  if (object$type %in% c(3, 4)) {
    "regression"
  } else {
    "classification"
  }
}


#' @keywords internal
superType.ksvm <- function(object) {
  if (grepl("svr$", object@type)) {
    "regression"
  } else if (grepl("svc$", object@type)) {
    "classification"
  } else {
    "other"
  }
}
