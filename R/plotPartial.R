#' Plotting Partial Dependence Functions
#'
#' Plots partial dependence functions using \code{lattice} graphics.
#'
#' @param x An object that inherits from the \code{"partial"} class.
#' @param smooth Logical indicating whether or not to overlay a LOESS smoother.
#'   Default is \code{FALSE}.
#' @param rug Logical indicating whether or not to include a rug representation
#'   to the plot. If \code{TRUE} the user must supply the original training data
#'   via the \code{train} option. Default is \code{FALSE}.
#' @param chull Logical indicating whether or not to draw the convex hull
#'   around the first two variables. If \code{TRUE} the user must supply the
#'   original training data via the \code{train} option.Default is \code{FALSE}.
#' @param contour Logical indicating whether or not to use
#'   \code{lattice::levelplot} (\code{TRUE}) or \code{lattice::wireframe}
#'   (\code{FALSE}). Default is \code{TRUE}.
#' @param number Integer specifying the number of conditional intervals for the
#'   panel variables.
#' @param overlap The fraction of overlap of the conditioning variables. See
#'   \code{?graphics::co.intervals} and \code{?lattice::equal.count} for further
#'   details.
#' @param train Data frame containing the original training data. Only
#'   required if \code{rug = TRUE} or \code{chull = TRUE}.
#' @param col.regions Color vector to be used if \code{contour} is \code{TRUE}.
#'   Defaults to the wonderful Matplotlib 'viridis' color map provided by the
#'   \code{viridis} package. See \code{?viridis::viridis} for details.
#' @param ... Additional optional arguments to be passed onto \code{levelplot},
#'   \code{wireframe}, or \code{xyplot}.
#'
#' @importFrom lattice dotplot equal.count levelplot panel.levelplot panel.lines
#' @importFrom lattice panel.loess panel.xyplot panel.rug wireframe xyplot
#'
#' @rdname plotPartial
#' @export
plotPartial <- function(x, ...) {
  UseMethod("plotPartial")
}


#' @rdname plotPartial
#' @export
plotPartial.partial <- function(x, smooth = FALSE, rug = FALSE, chull = FALSE,
                                contour = TRUE, number = 4, overlap = 0.1,
                                train = NULL, col.regions = viridis::viridis,
                                ...) {

  # Determine number of variables to plot
  nx <- ncol(x) - 1  # don't count response
  if (!(nx %in% 1:3)) {
    stop("Too many variables to plot. Try using lattice or ggplot2 directly.")
  }

  # Plot the partial dependence function
  if (nx == 1) {
    if (is.factor(x[[1L]])) {
      dotplot(stats::as.formula(paste("y ~", names(x)[1L])), data = x, ...)
    } else {
      xyplot(stats::as.formula(paste("y ~", names(x)[1L])), data = x,
             type = "l", ..., panel = function(xx, yy, ...) {
             # Add basic PDP
             panel.xyplot(xx, yy, col = "black", ...)
             # Add a loess smoother
             if (smooth) {
               panel.loess(xx, yy, ...)
             }
             # Add a rug display
             if (rug) {
               if (is.null(train)) {
                 stop("The training data must be supplied for rug display.")
               } else {
                  panel.rug(stats::quantile(train[[names(x)[1L]]],
                                            probs = 0:10/10, na.rm = TRUE))
               }
             }
      })
    }
  } else if (nx == 2) {
    form <- stats::as.formula(paste("y ~",
                                    paste(names(x)[1L:2L], collapse = "*")))
    if (contour) {
      levelplot(form, data = x, col.regions = col.regions, ...,
                panel = function(x1, y1, ...) {
                  panel.levelplot(x1, y1, ...)
                  if (rug || chull) {
                    if (is.null(train)) {
                      stop("The training data must be supplied for convex hull display.")
                    }
                  }
                  # Add a rug display
                  if (rug) {
                    panel.rug(stats::quantile(train[[names(x)[1L]]],
                                              probs = 0:10/10, na.rm = TRUE),
                              stats::quantile(train[[names(x)[2L]]],
                                              probs = 0:10/10, na.rm = TRUE),
                              col = "black")
                  }
                  # Plot the convex hull of the predictor space of interest
                  if (chull) {
                    if (is.null(train)) {
                      stop("The training data must be supplied for convex hull display.")
                    }
                    hpts <- grDevices::chull(na.omit(train[names(x)[1L:2L]]))
                    hpts <- c(hpts, hpts[1])
                    panel.lines(train[hpts, names(x)[1L:2L]],
                                col = "black")
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
    form <- stats::as.formula(paste("y ~",
                                    paste(names(x)[1L:2L], collapse = "*"), "|",
                                    paste(names(x)[3L:nx], collapse = "*")))
    if (contour) {
      levelplot(form, data = x, col.regions = col.regions, ...)
    } else {
      wireframe(form, data = x, ...)
    }
  }

}
