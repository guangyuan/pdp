#' @keywords internal
pdRegression <- function(object, pred.var, pred.grid, training.data, ...) {
  UseMethod("pdRegression")
}


#' @keywords internal
pdRegression.default <- function(object, pred.var, pred.grid, training.data,
                                 ...) {
  adply(pred.grid, .margins = 1, .fun = function(x) {
    temp <- training.data
    temp[pred.var] <- x
    mean(predict(object, newdata = temp), na.rm = TRUE)
  }, ...)
}
