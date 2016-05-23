#' Plotting Partial Dependence Functions
#'
#' Plots 1- and 2-dimensional partial dependence functions.
#'
#' @param x An object of class{"partial_1d"} or \code{"partial_2d"}.
#' @param contour Logical indicating whether or not to use \code{lattice::levelplot} 
#'   (\code{TRUE}) or \code{lattice::wireframe} (\code{FALSE}). Default is \code{TRUE}.
#' @param ... Additional optional arguments to be passed onto \code{lattice}
#'   plotting functions.
#' @export
plotPartial <- function(x, ...) {
  UseMethod("plotPartial")
}


plotPartial.partial_1d <- function(x, ...) {
  plot(x, ...)
}


plotPartial.partial_2d <- function(x, contour = TRUE, ...) {
  form <- as.formula(paste("y ~", paste(names(x)[1:2], collapse = "*")))
  if (contour) {
    levelplot(form, data = x, ...)
  } else {
    wireframe(form, data = x, ...)
  }
}
