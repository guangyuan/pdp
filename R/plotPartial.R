#' Plotting Partial Dependence Functions
#'
#' Plots 1- and 2-dimensional partial dependence functions.
#'
#' @param x An object of class{"partial_1d"} or \code{"partial_2d"}.
#' @param contour Logical indicating whether or not to use \code{lattice::levelplot} 
#'   (\code{TRUE}) or \code{lattice::wireframe} (\code{FALSE}). Default is \code{TRUE}.
#' @param ... Additional optional arguments to be passed onto \code{levelplot}, 
#'   \code{wireframe}, or \code{xyplot}.
#'
#' importFrom lattice levelplot wireframe xyplot
#' @rdname plotPartial
#' @export
plotPartial <- function(x, ...) {
  UseMethod("plotPartial")
}


#' @rdname plotPartial
#' @export
#' @method plotPartial partial_1d
plotPartial.partial_1d <- function(x, ...) {
  xyplot(as.formula(paste("y ~", names(x)[1L])), newdata = x, type = "l", ...)
}


#' @rdname plotPartial
#' @export
#' @method plotPartial partial_2d
plotPartial.partial_2d <- function(x, contour = TRUE, ...) {
  form <- as.formula(paste("y ~", paste(names(x)[1L:2L], collapse = "*")))
  if (contour) {
    levelplot(form, data = x, ...)
  } else {
    wireframe(form, data = x, ...)
  }
}
