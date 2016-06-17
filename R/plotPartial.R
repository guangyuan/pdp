#' Plotting Partial Dependence Functions
#'
#' Plots partial dependence functions using \code{lattice} graphics.
#'
#' @param x An object of class{"partial_1d"} or \code{"partial_2d"}.
#' @param smooth Logical
#' @param contour Logical indicating whether or not to use
#'   \code{lattice::levelplot} (\code{TRUE}) or \code{lattice::wireframe}
#'   (\code{FALSE}). Default is \code{TRUE}.
#' @param rug Logical indicating whether or not to include a rug representation
#'   to the plot. If \code{TRUE} the user must supply the original data.
#' @param convex.hull Logical
#' @param number Integer
#' @param overlap Proportion
#' @param ... Additional optional arguments to be passed onto \code{levelplot},
#'   \code{wireframe}, or \code{xyplot}.
#'
#' @importFrom lattice equal.count levelplot panel.lines panel.loess panel.xyplot panel.rug wireframe xyplot
#' @importFrom grDevices chull
#' @rdname plotPartial
#' @export
plotPartial <- function(x, ...) {
  UseMethod("plotPartial")
}


#' @rdname plotPartial
#' @export
plotPartial.partial <- function(x, smooth = FALSE, contour = TRUE, rug = FALSE,
                                convex.hull = FALSE, number = 4, overlap = 0.1,
                                training.data = NULL, ...) {

  # Determine number of variables to plot
  nx <- ncol(x) - 1  # don't count response
  if (!(nx %in% 1:3)) {
    stop("Too many variables to plot. Try using lattice or ggplot2 directly.")
  }

  # Plot the partial dependence function
  if (nx == 1) {
    xyplot(as.formula(paste("y ~", names(x)[1L])), data = x, type = "l", ...,
           panel = function(xx, yy, ...) {
             # Add basic PDP
             panel.xyplot(xx, yy, col = "black", ...)
             # Add a loess smoother
             if (smooth) {
               panel.loess(xx, yy, ...)
             }
             # Add a rug display
             if (rug) {
               if (is.null(training.data)) {
                 stop("The training data must be supplied for rug display.")
               } else {
                  panel.rug(quantile(training.data[[names(x)[1L]]], 
                                     probs = 0:10/10))               
               }
             }
    })
  } else if (nx == 2) {
    form <- as.formula(paste("y ~", paste(names(x)[1L:2L], collapse = "*")))
    if (contour) {
      levelplot(form, data = x, ...,
                panel = function(x1, y1, ...) {
                  panel.levelplot(x1, y1, ...)
                  if (convex.hull) {
                    if (is.null(training.data)) {
                      stop("The training data must be supplied for convex hull display.")
                    }
                    hpts <- chull(training.data[names(x)[1L:2L]])
                    hpts <- c(hpts, hpts[1])
                    panel.lines(training.data[hpts, names(x)[1L:2L]], col = "black")
                  }
                })
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
