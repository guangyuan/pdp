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
#' @param levelplot Logical indicating whether or not to use a false color level
#'   plot (\code{TRUE}) or a 3-D surface (\code{FALSE}). Default is \code{TRUE}.
#' @param contour Logical indicating whether or not to add contour lines to the
#'   level plot. Only used when \code{levelplot = TRUE}. Default is
#'   \code{FALSE}.
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
                                levelplot = TRUE, contour = FALSE, number = 4,
                                overlap = 0.1, train = NULL,
                                col.regions = viridis::viridis,
                                ...) {
  
  # Determine number of variables to plot
  nx <- ncol(x) - 1  # don't count response
  if (!(nx %in% 1:3)) {
    stop("Too many variables to plot. Try using lattice or ggplot2 directly.")
  }
  
  # Partial dependence plot for a single predictor
  if (nx == 1) {
    
    # If the predictor is a factor, then produce a dotplot
    if (is.factor(x[[1L]])) {
      
      # Produce a dotplot
      dotplot(stats::as.formula(paste("y ~", names(x)[1L])), data = x, ...)
      
    # If the predictor is continuous, then produce a lineplot
    } else {
      
      # Produce a lineplot
      xyplot(stats::as.formula(paste("y ~", names(x)[1L])), data = x, 
             type = "l", ..., panel = function(xx, yy, ...) {
               panel.xyplot(xx, yy, col = "black", ...)
               if (smooth) {
                 panel.loess(xx, yy, ...)
               }
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
    
  # Partial dependence plot for two predictors
  } else if (nx == 2) {
    
    # If both predictors are factors, then produce a paneled dotplot
    if (is.factor(x[[1L]]) && is.factor(x[[2L]])) {
      
      # FIXME: Should the factor with the smaller number of levels be used to
      #        define the panels?
      
      # Produce a paneled dotplot
      dotplot(stats::as.formula(paste("y ~", 
                                      paste(names(x)[1L:2L], collapse = "|"))), 
              data = x, ...)
    
    # If only one of the predictors is a factor, then produce a paneled lineplot
    } else if (is.factor(x[[1L]]) || is.factor(x[[2L]])) {
      
      # Lattice plot formula
      form <- if (is.factor(x[[1L]])) {
        stats::as.formula(paste("y ~", paste(names(x)[2L:1L], collapse = "|")))
      } else {
        stats::as.formula(paste("y ~", paste(names(x)[1L:2L], collapse = "|")))
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
                   panel.rug(stats::quantile(train[[names(x)[1L]]],
                                             probs = 0:10/10, na.rm = TRUE))
                 }
               }
             })
      
    # If both factors are continuous, then produce a false color level plot or
    # 3-D wireframe
    } else {
      
      # Lattice plot formula
      form <- stats::as.formula(paste("y ~",
                                      paste(names(x)[1L:2L], collapse = "*")))
      
      if (levelplot) {
        
        # False color level plot
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
                      hpts <- grDevices::chull(stats::na.omit(train[names(x)[1L:2L]]))
                      hpts <- c(hpts, hpts[1])
                      panel.lines(train[hpts, names(x)[1L:2L]],
                                  col = "black")
                    }
                  })
        
      } else {
        
        # Three-dimensional plot
        wireframe(form, data = x, ...)
        
      }
      
    }
    
  # Partial dependence plot for more than two predictors
  } else {
    
    # Convert additional predictors to factors using the equal count algorithm
    for (i in 3:nx) {
      if (!is.factor(x[[i]])) {
        x[[i]] <- equal.count(x[[i]], number = number, overlap = overlap)
      }
    }
    
    # If both predictors are factors, then produce a paneled dotplot
    if (is.factor(x[[1L]]) && is.factor(x[[2L]])) {
      
      # Lattice plot formula
      form <- stats::as.formula(paste("y ~", names(x)[1L], "|", 
                                      paste(names(x)[2L:nx], collapse = "*")))
      
      # Produce a paneled dotplot
      dotplot(form, data = x, ...)
      
    # If only one of the predictors is a factor, then produce a paneled lineplot
    } else if (is.factor(x[[1L]]) || is.factor(x[[2L]])) {
    
      # Lattice plot formula
      form <- if (is.factor(x[[1L]])) {
        stats::as.formula(paste("y ~", names(x)[2L], "|", 
                                paste(names(x)[c(1L, 3L:nx)], collapse = "*")))      
        } else {
          stats::as.formula(paste("y ~", names(x)[1L], "|", 
                                  paste(names(x)[2L:nx], collapse = "*"))) 
        }
      
      # FIXME: Should we average y over the shingles (if any) so that a single
      #        line can be produced in each panel?
      
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
                   panel.rug(stats::quantile(train[[names(x)[1L]]],
                                             probs = 0:10/10, na.rm = TRUE))
                 }
               }
             })
      
    # If both factors are continuous, then produce a false color level plot or
    # 3-D wireframe 
    } else {
      
      # Lattice plot formula
      form <- stats::as.formula(paste("y ~",
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

  }
  
}
