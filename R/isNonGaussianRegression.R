#' @keywords internal
isNonGaussianRegression <- function(object) {
  UseMethod("isNonGaussianRegression")
}


#' @keywords internal
isNonGaussianRegression.default <- function(object) {
  FALSE
}


#' @keywords internal
isNonGaussianRegression.earth <- function(object) {
  !is.null(object$glm.list)
}


#' @keywords internal
isNonGaussianRegression.glm <- function(object) {
  TRUE
}


#' @keywords internal
isNonGaussianRegression.gbm <- function(object) {
  object$distribution %in%
    c("gamma", "poisson", "tweedie")
}


#' @keywords internal
isNonGaussianRegression.xgb.Booster <- function(object) {
  object$params$objective %in%
    c("count:poisson", "reg:gamma")
}
