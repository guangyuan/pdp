#' Plotting Partial Dependence Functions
#'
#' Plots partial dependence functions (i.e., marginal effects) using
#' \code{\link[lattice]{lattice}} graphics.
#'
#' @param x An object that inherits from the \code{"partial"} class.
#'
#' @param center Logical indicating whether or not to produce centered ICE
#' curves (c-ICE curves). Only useful when \code{object} represents a set of ICE
#' curves; see \code{\link[pdp]{partial}} for details. Default is \code{FALSE}.
#'
#' @param plot.pdp Logical indicating whether or not to plot the partial
#' dependence function on top of the ICE curves. Default is \code{TRUE}.
#'
#' @param pdp.col Character string specifying the color to use for the partial
#' dependence function when \code{plot.pdp = TRUE}. Default is \code{"red"}.
#'
#' @param pdp.lwd Integer specifying the line width to use for the partial
#' dependence function when \code{plot.pdp = TRUE}. Default is \code{1}. See
#' \code{\link[graphics]{par}} for more details.
#'
#' @param pdp.lty Integer or character string specifying the line type to use
#' for the partial dependence function when  \code{plot.pdp = TRUE}. Default is
#' \code{1}. See \code{\link[graphics]{par}} for more details.
#'
#' @param smooth Logical indicating whether or not to overlay a LOESS smooth.
#' Default is \code{FALSE}.
#'
#' @param rug Logical indicating whether or not to include rug marks on the
#' predictor axes. Default is \code{FALSE}.
#'
#' @param chull Logical indicating wether or not to restrict the first two
#' variables in \code{pred.var} to lie within the convex hull of their training
#' values; this affects \code{pred.grid}. Default is \code{FALSE}.
#'
#' @param levelplot Logical indicating whether or not to use a false color level
#' plot (\code{TRUE}) or a 3-D surface (\code{FALSE}). Default is \code{TRUE}.
#'
#' @param contour Logical indicating whether or not to add contour lines to the
#' level plot. Only used when \code{levelplot = TRUE}. Default is \code{FALSE}.
#'
#' @param number Integer specifying the number of conditional intervals to use
#' for the continuous panel variables. See \code{\link[graphics]{co.intervals}}
#' and \code{\link[lattice]{equal.count}} for further details.
#'
#' @param overlap The fraction of overlap of the conditioning variables. See
#' \code{\link[graphics]{co.intervals}} and \code{\link[lattice]{equal.count}}
#' for further details.
#'
#' @param train Data frame containing the original training data. Only required
#' if \code{rug = TRUE} or \code{chull = TRUE}.
#'
#' @param col.regions Color vector to be used if \code{levelplot} is
#' \code{TRUE}. Defaults to the wonderful Matplotlib 'viridis' color map
#' provided by the \code{viridis} package. See \code{\link[viridis]{viridis}}
#' for details.
#'
#' @param ... Additional optional arguments to be passed onto \code{dotplot},
#' \code{levelplot}, \code{xyplot}, or \code{wireframe}.
#'
#' @importFrom lattice dotplot equal.count levelplot panel.dotplot
#'
#' @importFrom lattice panel.levelplot panel.lines panel.loess panel.xyplot
#'
#' @importFrom lattice panel.rug wireframe xyplot
#'
#' @rdname plotPartial
#'
#' @export
#'
#' @examples
#' # See ?partial for examples
#' ?partial
plotPartial <- function(x, ...) {
  UseMethod("plotPartial")
}


#' @rdname plotPartial
#' @export
plotPartial.ice <- function(x, center = FALSE, plot.pdp = TRUE,
                            pdp.col = "red2", pdp.lwd = 2, pdp.lty = 1,
                            rug = FALSE, train = NULL, ...) {
  plotIceCurves(x, center = center, plot.pdp = plot.pdp, pdp.col = pdp.col,
                pdp.lwd = pdp.lwd, pdp.lty = pdp.lty, rug = rug, train = train,
                ...)
}


#' @rdname plotPartial
#' @export
plotPartial.cice <- function(x, plot.pdp = TRUE, pdp.col = "red2", pdp.lwd = 2,
                             pdp.lty = 1, rug = FALSE, train = NULL, ...) {
  plotCIceCurves(x, plot.pdp = plot.pdp, pdp.col = pdp.col, pdp.lwd = pdp.lwd,
                 pdp.lty = pdp.lty, rug = rug, train = train, ...)
}


#' @rdname plotPartial
#' @export
plotPartial.partial <- function(x, center = FALSE, plot.pdp = TRUE,
                                pdp.col = "red2", pdp.lwd = 2, pdp.lty = 1,
                                smooth = FALSE, rug = FALSE, chull = FALSE,
                                levelplot = TRUE, contour = FALSE, number = 4,
                                overlap = 0.1, train = NULL,
                                col.regions = viridis::viridis,
                                ...) {

  # Determine of x contains multiple PDPs
  multi <- if ("yhat.id" %in% names(x)) {
    TRUE
  } else {
    FALSE
  }

  # Determine number of variables to plot
  nx <- if (multi) {
    ncol(x) - 2  # don't count yhat or yhat.id
  } else {
    ncol(x) - 1  # don't count yhat
  }

  # Throw error if too difficult to plot
  if ((!multi && !(nx %in% 1:3)) || (multi && (nx > 1))) {
    stop("Too many variables to plot. Try using lattice or ggplot2 directly.")
  }

  # Determine which type of plot to produce
  if (multi) {

    # Multiple PDPs for a single predictor
    p <- plotMultiPDFs(x, center = center, plot.pdp = plot.pdp,
                       pdp.col = pdp.col, pdp.lwd = pdp.lwd, pdp.lty = pdp.lty,
                       rug = rug, train = train, ...)

  } else if (nx == 1L) {

    # PDP for a single predictor
    p <- if (is.factor(x[[1L]])) {
      plotFacPDF(x, ...)
    } else {
      plotNumPDF(x, rug = rug, smooth = smooth, train = train, ...)
    }

  } else if (nx == 2) {

    # PDP for two predictors
    p <- if (is.factor(x[[1L]]) && is.factor(x[[2L]])) {
      plotFacFacPDF(x, ...)
    } else if (is.factor(x[[1L]]) || is.factor(x[[2L]])) {
      plotNumFacPDF(x, smooth = smooth, rug = rug, train = train, ...)
    } else {
      plotNumNumPDF(x, levelplot = levelplot, contour = contour, rug = rug,
                    chull = chull, train = train, col.regions = col.regions,
                    ...)
    }

  } else {

    # Convert additional predictors to factors using the equal count algorithm
    for (i in 3:nx) {
      if (!is.factor(x[[i]])) {
        x[[i]] <- equal.count(x[[i]], number = number, overlap = overlap)
      }
    }

    # PDP for more than two predictors
    p <- if (is.factor(x[[1L]]) && is.factor(x[[2L]])) {
      plotFacFacShiPDF(x, nx = nx, ...)
    } else if (is.factor(x[[1L]]) || is.factor(x[[2L]])) {
      plotNumFacShiPDF(x, nx = nx, smooth = smooth, rug = rug, train = train,
                       ...)
    } else {
      plotNumNumShiPDF(x, nx = nx, levelplot = levelplot, contour = contour,
                       col.regions = col.regions, ...)

    }

  }

  # Print and return (invisibly) the "trellis" object
  p

}


#' @keywords internal
plotIceCurves <- function(x, plot.pdp, center, pdp.col, pdp.lwd, pdp.lty, rug,
                          train, ...) {
  if (is.factor(x[[1L]])) {
    dotplot(stats::as.formula(paste("yhat ~", names(x)[1L])), data = x,
            groups = x$yhat.id, type = "l", ...,
            panel = function(xx, yy, ...) {
              panel.dotplot(xx, yy, col = "black", ...)
              if (rug) {
                if (is.null(train)) {
                  stop("The training data must be supplied for rug display.")
                } else {
                  panel.rug(stats::quantile(train[, names(x)[1L]],
                                            probs = 0:10/10, na.rm = TRUE))
                }
              }
            })
  } else {
    if (center) {  # c-ICE curves
      x <- x %>%
        dplyr::group_by_("yhat.id") %>%
        dplyr::mutate_("yhat" = "yhat - first(yhat)") %>%
        dplyr::ungroup()
    }
    xyplot(stats::as.formula(paste("yhat ~", names(x)[1L])), data = x,
           groups = x$yhat.id, type = "l", ...,
           panel = function(xx, yy, ...) {
             panel.xyplot(xx, yy, col = "black", ...)
             if (plot.pdp) {
               pd <- averageIceCurves(x)
               panel.xyplot(pd$x, pd$yhat, type = "l", col = pdp.col,
                            lwd = pdp.lwd, lty = pdp.lty)
             }
             if (rug) {
               if (is.null(train)) {
                 stop("The training data must be supplied for rug display.")
               } else {
                 panel.rug(stats::quantile(train[, names(x)[1L]],
                                           probs = 0:10/10, na.rm = TRUE))
               }
             }
           })
  }
}


#' @keywords internal
plotCIceCurves <- function(x, plot.pdp, pdp.col, pdp.lwd, pdp.lty, rug, train,
                           ...) {
  if (is.factor(x[[1L]])) {
    dotplot(stats::as.formula(paste("yhat ~", names(x)[1L])), data = x,
            groups = x$yhat.id, type = "l", ...,
            panel = function(xx, yy, ...) {
              panel.dotplot(xx, yy, col = "black", ...)
              if (rug) {
                if (is.null(train)) {
                  stop("The training data must be supplied for rug display.")
                } else {
                  panel.rug(stats::quantile(train[, names(x)[1L]],
                                            probs = 0:10/10, na.rm = TRUE))
                }
              }
            })
  } else {
    xyplot(stats::as.formula(paste("yhat ~", names(x)[1L])), data = x,
           groups = x$yhat.id, type = "l", ...,
           panel = function(xx, yy, ...) {
             panel.xyplot(xx, yy, col = "black", ...)
             if (plot.pdp) {
               pd <- averageIceCurves(x)
               panel.xyplot(pd$x, pd$yhat, type = "l", col = pdp.col,
                            lwd = pdp.lwd, lty = pdp.lty)
             }
             if (rug) {
               if (is.null(train)) {
                 stop("The training data must be supplied for rug display.")
               } else {
                 panel.rug(stats::quantile(train[, names(x)[1L]],
                                           probs = 0:10/10, na.rm = TRUE))
               }
             }
           })
  }
}


#' @keywords internal
plotMultiPDFs <- function(x, plot.pdp, center, pdp.col, pdp.lwd, pdp.lty, rug,
                          train, ...) {
  if (is.factor(x[[1L]])) {
    dotplot(stats::as.formula(paste("yhat ~", names(x)[1L])), data = x,
            groups = x$yhat.id, type = "l", ...,
            panel = function(xx, yy, ...) {
              panel.dotplot(xx, yy, col = "black", ...)
              if (rug) {
                if (is.null(train)) {
                  stop("The training data must be supplied for rug display.")
                } else {
                  panel.rug(stats::quantile(train[, names(x)[1L]],
                                            probs = 0:10/10, na.rm = TRUE))
                }
              }
            })
  } else {
    if (center) {
      x <- x %>%
        dplyr::group_by_("yhat.id") %>%
        dplyr::mutate_(yhat = "yhat - first(yhat)") %>%
        dplyr::ungroup()
    }
    xyplot(stats::as.formula(paste("yhat ~", names(x)[1L])), data = x,
           groups = x$yhat.id, type = "l", ...,
           panel = function(xx, yy, ...) {
             panel.xyplot(xx, yy, col = "black", ...)
             if (plot.pdp) {
               pd <- averageIceCurves(x)
               panel.xyplot(pd$x, pd$yhat, type = "l", col = pdp.col,
                            lwd = pdp.lwd, lty = pdp.lty)
             }
             if (rug) {
               if (is.null(train)) {
                 stop("The training data must be supplied for rug display.")
               } else {
                 panel.rug(stats::quantile(train[, names(x)[1L]],
                                           probs = 0:10/10, na.rm = TRUE))
               }
             }
           })
  }
}


#' @keywords internal
plotNumPDF <- function(x, smooth, rug, train = NULL, ...) {
  xyplot(stats::as.formula(paste("yhat ~", names(x)[1L])), data = x,
         type = "l", ..., panel = function(xx, yy, ...) {
           panel.xyplot(xx, yy, col = "black", ...)
           if (smooth) {
             panel.loess(xx, yy, ...)
           }
           if (rug) {
             if (is.null(train)) {
               stop("The training data must be supplied for rug display.")
             } else {
               panel.rug(stats::quantile(train[, names(x)[1L]],
                                         probs = 0:10/10, na.rm = TRUE))
             }
           }
         })
}


#' @keywords internal
plotFacPDF <- function(x, ...) {
  dotplot(stats::as.formula(paste("yhat ~", names(x)[1L])), data = x, ...)
}


#' @keywords internal
plotFacFacPDF <- function(x, ...) {
  dotplot(stats::as.formula(paste("yhat ~",
                                  paste(names(x)[1L:2L], collapse = "|"))),
          data = x, ...)
}


#' @keywords internal
plotNumFacPDF <- function(x, smooth, rug, train, ...) {

  # Lattice plot formula
  form <- if (is.factor(x[[1L]])) {
    stats::as.formula(paste("yhat ~", paste(names(x)[2L:1L], collapse = "|")))
  } else {
    stats::as.formula(paste("yhat ~", paste(names(x)[1L:2L], collapse = "|")))
  }

  # Produce a paneled lineplot
  xyplot(form, data = x, type = "l", ...,
         panel = function(xx, yy, ...) {
           panel.xyplot(xx, yy, col = "black", ...)
           if (smooth) {
             panel.loess(xx, yy, ...)
           }
           if (rug) {
             if (is.null(train)) {
               stop("The training data must be supplied for rug display.")
             } else {
               panel.rug(stats::quantile(train[, names(x)[1L]],
                                         probs = 0:10/10, na.rm = TRUE))
             }
           }
         })

}


#' @keywords internal
plotNumNumPDF <- function(x, levelplot, rug, chull, train, contour, col.regions,
                          ...) {

  # Lattice plot formula
  form <- stats::as.formula(paste("yhat ~",
                                  paste(names(x)[1L:2L], collapse = "*")))

  # False color level plot
  if (levelplot) {

    # Lattice-based false color level plot
    levelplot(form, data = x, col.regions = col.regions, contour = contour,
              ...,
              panel = function(x1, y1, ...) {
                panel.levelplot(x1, y1, ...)
                if (rug || chull) {
                  if (is.null(train)) {
                    stop("The training data must be supplied for convex hull display.")
                  }
                }
                # Add a rug display
                if (rug) {
                  panel.rug(stats::quantile(train[, names(x)[1L]],
                                            probs = 0:10/10, na.rm = TRUE),
                            stats::quantile(train[, names(x)[2L]],
                                            probs = 0:10/10, na.rm = TRUE),
                            col = "black")
                }
                # Plot the convex hull of the predictor space of interest
                if (chull) {
                  if (is.null(train)) {
                    stop("The training data must be supplied for convex hull display.")
                  }
                  hpts <- grDevices::chull(stats::na.omit(train[names(x)[1L:2L]]))
                  hpts <- c(hpts, hpts[1])
                  panel.lines(train[hpts, names(x)[1L:2L]],
                              col = "black")
                }
              })

    # Wireframe
  } else {

    # Lattice-based three-dimensional wireframe plot
    wireframe(form, data = x, ...)

  }

}


#' @keywords internal
plotFacFacShiPDF <- function(x, nx, ...) {

  # Produce a paneled dotplot
  dotplot(stats::as.formula(paste("yhat ~", names(x)[1L], "|",
                                  paste(names(x)[2L:nx], collapse = "*"))),
          data = x, ...)

}


#' @keywords internal
plotNumFacShiPDF <- function(x, nx, smooth, rug, train, ...) {

  # Lattice plot formula
  form <- if (is.factor(x[[1L]])) {
    stats::as.formula(paste("yhat ~", names(x)[2L], "|",
                            paste(names(x)[c(1L, 3L:nx)], collapse = "*")))
  } else {
    stats::as.formula(paste("yhat ~", names(x)[1L], "|",
                            paste(names(x)[2L:nx], collapse = "*")))
  }

  # Produce a paneled lineplot
  xyplot(form, data = x, type = "p", ...,
         panel = function(xx, yy, ...) {
           panel.xyplot(xx, yy, col = "black", ...)
           if (smooth) {
             panel.loess(xx, yy, ...)
           }
           if (rug) {
             if (is.null(train)) {
               stop("The training data must be supplied for rug display.")
             } else {
               panel.rug(stats::quantile(train[, names(x)[1L]],
                                         probs = 0:10/10, na.rm = TRUE))
             }
           }
         })

}


#' @keywords internal
plotNumNumShiPDF <- function(x, nx, levelplot, contour, col.regions, ...) {

  # Lattice plot formula
  form <- stats::as.formula(paste("yhat ~",
                                  paste(names(x)[1L:2L], collapse = "*"),
                                  "|",
                                  paste(names(x)[3L:nx], collapse = "*")))

  # Produce a false color level plot or three-dimensional plot
  if (levelplot) {
    levelplot(form, data = x, col.regions = col.regions, contour = contour,
              ...)
  } else {
    wireframe(form, data = x, ...)
  }

}
