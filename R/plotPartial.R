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
plotPartial.partial <- function(x, contour = TRUE, number = 4, overlap = 0.1, ...) {
  
  # Determine number of variables to plot
  nx <- ncol(x) - 1  # don't count response
  if (!(nx %in% 1:3)) {
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
    for (i in 3:nx) {
      if (!is.factor(x[[i]])) {
        x[[i]] <- equal.count(x[[i]], number = number, overlap = overlap)
      }
    }
    form <- as.formula(paste("y ~", paste(names(x)[1L:2L], collapse = "*"), "|", 
                             paste(names(x)[3L:nx], collapse = "*")))
    if (contour) {
      levelplot(form, data = x, ...)
    } else {
      wireframe(form, data = x, ...)
    }
  }

}
