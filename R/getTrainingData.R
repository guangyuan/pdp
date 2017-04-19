# Error message to display when training data cannot be extracted form object
mssg <- paste0("The training data could not be extracted from object. Please ",
               "supply the raw training data using the `train` argument in ",
               "the call to `partial`.")



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
getTrainingData.cforest <- function(object) {
  stop(mssg)
}


#' @keywords internal
getTrainingData.ctree <- function(object) {
  stop(mssg)
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
