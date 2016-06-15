#' Plotting Partial Dependence Functions
#'
#' Plots partial dependence functions using \code{lattice} graphics.
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
plotPartial.partial <- function(x, contour = TRUE, ...) {
  
  # Determine number of variables to plot
  nx <- ncol(x) - 1  # don't count response
  if (!(nx %in% 2:4)) {
    stop("Too many variables to plot. Try using lattice or ggplot2 directly.")
  }
  
  # Plot the partial dependence function
  if (nx == 1) {
    xyplot(as.formula(paste("y ~", names(x)[1L])), newdata = x, type = "l", ...)
  } else if (nx == 2) {
    form <- as.formula(paste("y ~", paste(names(x)[1L:2L], collapse = "*")))
    if (contour) {
      levelplot(form, data = x, ...)
    } else {
      wireframe(form, data = x, ...)
    }
  } else {
    form <- as.formula(paste("y ~", paste(names(x)[1L:2L], collapse = "*"), "|", 
                             paste(names(x)[3L:nx], collapse = "*")))
    if (contour) {
      levelplot(form, data = x, ...)
    } else {
      wireframe(form, data = x, ...)
    }
  }

}


#' @rdname plotPartial
#' @export
plotPartial.partial_1d <- function(x, ...) {
  xyplot(as.formula(paste("y ~", names(x)[1L])), newdata = x, type = "l", ...)
}


#' @rdname plotPartial
#' @export
plotPartial.partial_2d <- function(x, contour = TRUE, ...) {
  form <- as.formula(paste("y ~", paste(names(x)[1L:2L], collapse = "*")))
  if (contour) {
    levelplot(form, data = x, ...)
  } else {
    wireframe(form, data = x, ...)
  }
}
