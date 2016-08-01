#' @keywords internal
pdRegression <- function(object, pred.var, pred.grid, train, ...) {
  UseMethod("pdRegression")
}


#' @keywords internal
pdRegression.default <- function(object, pred.var, pred.grid, train,
                                 ...) {
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    mean(stats::predict(object, newdata = temp), na.rm = TRUE)
  }, ...)
}


#' @keywords internal
#' @importFrom utils capture.output
pdRegression.gbm <- function(object, pred.var, pred.grid, train,
                                 ...) {
  # Necessary to avoid silly printing from predict.gbm
  plyr::adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- train
    temp[pred.var] <- x
    log <- capture.output(z <- mean(stats::predict(object, newdata = temp),
                                    na.rm = TRUE))
    z
  }, ...)
}
