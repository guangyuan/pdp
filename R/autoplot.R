#' Plotting Partial Dependence Functions
#'
#' Plots partial dependence functions (i.e., marginal effects) using
#' \code{\link[ggplot2]{ggplot2}} graphics.
#'
#' @param object An object that inherits from the \code{"partial"} class.
#'
#' @param center Logical indicating whether or not to produce centered ICE
#' curves (c-ICE curves). Only useful when \code{object} represents a set of ICE
#' curves; see \code{\link[pdp]{partial}} for details. Default is \code{FALSE}.
#'
#' @param plot.pdp Logical indicating whether or not to plot the partial
#' dependence function on top of the ICE curves. Default is \code{TRUE}.
#'
#' @param pdp.color Character string specifying the color to use for the partial
#' dependence function when \code{plot.pdp = TRUE}. Default is \code{"red"}.
#'
#' @param pdp.size Positive number specifying the line width to use for the
#' partial dependence function when \code{plot.pdp = TRUE}. Default is \code{1}.
#'
#' @param pdp.linetype Positive number specifying the line type to use for the
#' partial dependence function when \code{plot.pdp = TRUE}. Default is \code{1}.
#'
#' @param rug Logical indicating whether or not to include rug marks on the
#' predictor axes. Default is \code{FALSE}.
#'
#' @param smooth Logical indicating whether or not to overlay a LOESS smooth.
#' Default is \code{FALSE}.
#'
#' @param smooth.method Character string specifying the smoothing method
#' (function) to use (e.g., \code{"auto"}, \code{"lm"}, \code{"glm"},
#' \code{"gam"}, \code{"loess"}, or \code{"rlm"}). Default is \code{"auto"}.
#' See \code{\link[ggplot2]{geom_smooth}} for details.
#'
#' @param smooth.formula Formula to use in smoothing function (e.g.,
#' \code{y ~ x}, \code{y ~ poly(x, 2)}, or \code{y ~ log(x)}).
#'
#' @param smooth.span Controls the amount of smoothing for the default loess
#' smoother. Smaller numbers produce wigglier lines, larger numbers produce
#' smoother lines. Default is \code{0.75}.
#'
#' @param smooth.method.args List containing additional arguments to be passed
#' on to the modelling function defined by \code{smooth.method}.
#'
#' @param contour Logical indicating whether or not to add contour lines to the
#' level plot. Only used when \code{levelplot = TRUE}. Default is \code{FALSE}.
#'
#' @param contour.color Character string specifying the color to use for the
#' contour lines when \code{contour = TRUE}. Default is \code{"white"}.
#'
#' @param palette If a string, will use that named palette. If a number, will
#' index into the list of palettes of appropriate type. Default is
#' \code{"Spectral"}.
#'
#' @param train Data frame containing the original training data. Only required
#' if \code{rug = TRUE} or \code{chull = TRUE}.
#'
#' @param xlab Charater string specifying the text for the x-axis label.
#'
#' @param ylab Charater string specifying the text for the y-axis label.
#'
#' @param main Character string specifying the text for the main title of the
#' plot.
#'
#' @param legend.title Charater string specifying the text for the legend title.
#' Default is \code{"yhat"}.
#'
#' @param ... Additional optional arguments to be passed onto \code{geom_line}.
#'
#' @return A \code{"ggplot"} object.
#'
#' @importFrom ggplot2 aes_string autoplot facet_wrap geom_contour geom_line
#'
#' @importFrom ggplot2 geom_point geom_rug geom_smooth geom_tile ggplot ggtitle
#'
#' @importFrom ggplot2 scale_fill_distiller stat_summary theme_bw xlab ylab
#'
#' @rdname autoplot.partial
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #
#' # Regression example (requires randomForest package to run)
#' #
#'
#' # Load required packages
#' library(ggplot2)  # required to use autoplot
#' library(randomForest)
#'
#' # Fit a random forest to the Boston housing data
#' data (boston)  # load the boston housing data
#' set.seed(101)  # for reproducibility
#' boston.rf <- randomForest(cmedv ~ ., data = boston)
#'
#' # Partial dependence of cmedv on lstat
#' boston.rf %>%
#'   partial(pred.var = "lstat") %>%
#'   autoplot(rug = TRUE, train = boston)
#'
#' # Partial dependence of cmedv on lstat and rm
#' boston.rf %>%
#'   partial(pred.var = c("lstat", "rm"), chull = TRUE, progress = "text") %>%
#'   autoplot(contour = TRUE, legend.title = "rm")
#'
#' # ICE curves and c-ICE curves
#' age.ice <- partial(boston.rf, pred.var = "lstat", ice = TRUE)
#' grid.arrange(
#'   autoplot(age.ice, alpha = 0.5),                 # ICE curves
#'   autoplot(age.ice, center = TRUE, alpha = 0.5),  # c-ICE curves
#'   ncol = 2
#' )
#' }
autoplot.partial <- function(object, center = FALSE, plot.pdp = TRUE,
                             pdp.color = "red", pdp.size = 1, pdp.linetype = 1,
                             rug = FALSE, smooth = FALSE,
                             smooth.method = "auto", smooth.formula = y ~ x,
                             smooth.span = 0.75, smooth.method.args = list(),
                             contour = FALSE, contour.color = "white",
                             palette = "Spectral", train = NULL,
                             xlab = NULL, ylab = NULL, main = NULL,
                             legend.title = NULL, ...) {

  # Determine if object contains an ID column (i.e., multiple curves)
  multi <- "yhat.id" %in% names(object)

  # Determine number of variables to plot
  nx <- if (multi) {
    ncol(object) - 2  # don't count yhat or yhat.id
  } else {
    ncol(object) - 1  # don't count yhat
  }

  # Generate plot
  if (multi) {
    ggPlotIceCurves(object, center = center, plot.pdp = plot.pdp,
                    pdp.color = pdp.color, pdp.size = pdp.size,
                    pdp.linetype = 1, rug = rug, train = train, xlab = xlab,
                    ylab = ylab, main = main, ...)
  } else if (nx == 1L) {  # single predictor
    ggPlotOnePredictorPDP(object, rug = rug, smooth = smooth,
                          smooth.method = smooth.method,
                          smooth.formula = smooth.formula,
                          smooth.span = smooth.span,
                          smooth.method.args = smooth.method.args,
                          train = train, xlab = xlab, ylab = ylab, main = main,
                          ...)
  } else if (nx == 2L) {  # two predictors
    ggPlotTwoPredictorPDP(object, rug = rug, smooth = smooth,
                          smooth.method = smooth.method,
                          smooth.formula = smooth.formula,
                          smooth.span = smooth.span,
                          smooth.method.args = smooth.method.args,
                          contour = contour, contour.color = contour.color,
                          palette = palette, train = train, xlab = xlab,
                          ylab = ylab, main = main, legend.title = legend.title,
                          ...)
  } else {  # more than two predictors
    stop("autoplot does not currently support more than two predictors")
  }

}


#' @rdname autoplot.partial
#' @export
autoplot.ice <- function(object, center = FALSE, plot.pdp = TRUE,
                         pdp.color = "red", pdp.size = 1, pdp.linetype = 1,
                         rug = FALSE, train = NULL, xlab = NULL, ylab = NULL,
                         main = NULL, ...) {
  ggPlotIceCurves(object, center = center, plot.pdp = plot.pdp,
                  pdp.color = pdp.color, pdp.size = pdp.size,
                  pdp.linetype = pdp.linetype, rug = rug, train = train,
                  xlab = xlab, ylab = ylab, main = main, ...)
}


#' @rdname autoplot.partial
#' @export
autoplot.cice <- function(object, plot.pdp = TRUE, pdp.color = "red",
                          pdp.size = 1, pdp.linetype = 1, rug = FALSE,
                          train = NULL, xlab = NULL, ylab = NULL, main = NULL,
                          ...) {
  ggPlotIceCurves(object, center = FALSE, plot.pdp = plot.pdp,
                  pdp.color = pdp.color, pdp.size = pdp.size,
                  pdp.linetype = pdp.linetype, rug = rug, train = train,
                  xlab = xlab, ylab = ylab, main = main, ...)
}


#' @keywords internal
ggPlotIceCurves <- function(object, center, plot.pdp, pdp.color, pdp.size,
                            pdp.linetype, rug, train, xlab, ylab, main, ...) {

  # Should the curves be centered to start at yhat = 0?
  if (center) {
    object <- centerIceCurves(object)
  }

  # Use the first column to determine which type of plot to construct
  if (is.factor(object[[1L]])) {

    # Draw scatterplots
    p <- ggplot(object,
                aes_string(x = names(object)[[1L]], y = "yhat", group = 1)) +
      geom_line(aes_string(group = "yhat.id"), ...) +
      geom_point(aes_string(group = "yhat.id"), alpha = 1)

    # Should the PDP be displayed too?
    if (plot.pdp) {
      p <- p + stat_summary(fun.y = mean, geom = "line", col = pdp.color,
                            size = pdp.size, linetype = pdp.linetype)
    }

  } else {

    # Draw lineplots
    p <- ggplot(object, aes_string(x = names(object)[[1L]], y = "yhat")) +
      geom_line(aes_string(group = "yhat.id"), ...)

    # Should the PDP be displayed too?
    if (plot.pdp) {
      p <- p + stat_summary(fun.y = mean, geom = "line", col = pdp.color,
                            size = pdp.size, linetype = pdp.linetype)
    }

    # Add rug plot to x-axis
    if (rug) {
      if (is.null(train)) {
        stop("The training data must be supplied for rug display.")
      } else {
        x.name <- which(names(train) == names(object)[[1L]])
        x.rug <- data.frame(as.numeric(
          stats::quantile(train[, x.name, drop = TRUE], probs = 0:10/10,
                          na.rm = TRUE)))
        p <- p + geom_rug(data = x.rug, aes_string(x = names(x.rug)[1L]),
                          sides = "b", inherit.aes = FALSE)
      }
    }

  }

  # Add axis labels and title
  if (is.null(xlab)) {
    p <- p + xlab(names(object)[1L])
  } else {
    p <- p + xlab(xlab)
  }
  if (is.null(ylab)) {
    p <- p + ylab("yhat")
  } else {
    p <- p + ylab(ylab)
  }
  if (!is.null(main)) {
    p <- p + ggtitle(main)
  }

  # Return "ggplot" object
  p

}


#' @keywords internal
ggPlotOnePredictorPDP <- function(object, rug, smooth, smooth.method,
                                  smooth.formula, smooth.span,
                                  smooth.method.args, train, xlab, ylab, main,
                                  ...) {

  # Use the first column to determine which type of plot to construct
  if (is.factor(object[[1L]])) {

    # Draw a scatterplot
    p <- ggplot(object, aes_string(x = names(object)[[1L]], y = "yhat")) +
      geom_point(...)

  } else {

    # Draw a lineplot
    p <- ggplot(object, aes_string(x = names(object)[[1L]], y = "yhat")) +
      geom_line(...)

    # Add rug plot to x-axis
    if (rug) {
      if (is.null(train)) {
        stop("The training data must be supplied for rug display.")
      } else {
        x.name <- which(names(train) == names(object)[[1L]])
        x.rug <- data.frame(as.numeric(
          stats::quantile(train[, x.name, drop = TRUE], probs = 0:10/10,
                          na.rm = TRUE)))
        p <- p + geom_rug(data = x.rug, aes_string(x = names(x.rug)[1L]),
                          sides = "b", inherit.aes = FALSE)
      }
    }

    # Add smoother
    if (smooth) {
      p <- p + geom_smooth(method = smooth.method, formula = smooth.formula,
                           span = smooth.span, se = FALSE,
                           method.args = smooth.method.args)
    }

  }

  # Add axis labels and title
  if (is.null(xlab)) {
    p <- p + xlab(names(object)[1L])
  } else {
    p <- p + xlab(xlab)
  }
  if (is.null(ylab)) {
    p <- p + ylab("yhat")
  } else {
    p <- p + ylab(ylab)
  }
  if (!is.null(main)) {
    p <- p + ggtitle(main)
  }

  # Return "ggplot" object
  p

}


#' @keywords internal
ggPlotTwoPredictorPDP <- function(object, rug, smooth, smooth.method,
                                  smooth.formula, smooth.span,
                                  smooth.method.args, contour, contour.color,
                                  palette, train, xlab, ylab, main,
                                  legend.title, ...) {

  # Use the first two columns to determine which type of plot to construct
  if (is.factor(object[[1L]]) && is.factor(object[[2L]])) {

    # Draw a faceted scatterplot
    p <- ggplot(object, aes_string(x = names(object)[[1L]], y = "yhat")) +
      geom_point(...) +
      facet_wrap(~ object[[2L]])

  } else if (is.factor(object[[1L]]) && !is.factor(object[[2L]])) {

    # Draw a faceted lineplot
    p <- ggplot(object, aes_string(x = names(object)[[2L]], y = "yhat")) +
      geom_line(...) +
      facet_wrap(~ object[[1L]])

    # Add rug plot to the x-axis
    if (rug) {
      if (is.null(train)) {
        stop("The training data must be supplied for rug display.")
      } else {
        x.name <- which(names(train) == names(object)[[2L]])
        x.rug <- data.frame(as.numeric(
          stats::quantile(train[, x.name, drop = TRUE], probs = 0:10/10,
                          na.rm = TRUE)))
        p <- p + geom_rug(data = x.rug, aes_string(x = names(x.rug)[1L]),
                          sides = "b", inherit.aes = FALSE)
      }
    }

    # Add smoother
    if (smooth) {
      p <- p + geom_smooth(method = smooth.method, formula = smooth.formula,
                           span = smooth.span, se = FALSE,
                           method.args = smooth.method.args)
    }

  } else if (!is.factor(object[[1L]]) && is.factor(object[[2L]])) {

    # Draw a faceted lineplot
    p <- ggplot(object, aes_string(x = names(object)[[1L]], y = "yhat")) +
      geom_line(...) +
      facet_wrap(~ object[[2L]])

    # Add rug plot to x-axis
    if (rug) {
      if (is.null(train)) {
        stop("The training data must be supplied for rug display.")
      } else {
        x.name <- which(names(train) == names(object)[[1L]])
        x.rug <- data.frame(as.numeric(
          stats::quantile(train[, x.name, drop = TRUE], probs = 0:10/10,
                          na.rm = TRUE)))
        p <- p + geom_rug(data = x.rug, aes_string(x = names(x.rug)[1L]),
                          sides = "b", inherit.aes = FALSE)
      }
    }

    # Add smoother
    if (smooth) {
      p <- p + geom_smooth(method = smooth.method, formula = smooth.formula,
                           span = smooth.span, se = FALSE,
                           method.args = smooth.method.args)
    }

  } else {

    # Draw a false color level plot
    p <- ggplot(object, aes_string(x = names(object)[[1L]],
                                   y = names(object)[[2L]],
                                   z = "yhat",
                                   fill = "yhat")) +
      geom_tile()

    # Add contour lines
    if (contour) {
      p <- p + geom_contour(color = contour.color)
    }

    # Add legend title and theme
    if (is.null(legend.title)) {
      p <- p + scale_fill_distiller(name = "yhat", palette = palette)
    } else {
      p <- p + scale_fill_distiller(name = legend.title, palette = palette)
    }
    p <- p + theme_bw()

  }


  # Add axis labels and title
  if (is.null(xlab)) {
    p <- if (is.factor(object[[1L]]) && !is.factor(object[[2L]])) {
      p + xlab(names(object)[2L])
    } else {
      p + xlab(names(object)[1L])
    }
  } else {
    p <- p + xlab(xlab)
  }
  if (is.null(ylab)) {
    p <- if (!is.factor(object[[1L]]) && !is.factor(object[[2L]])) {
      p + ylab(names(object)[2L])
    } else {
      p + ylab("yhat")
    }
  } else {
    p <- p + ylab(ylab)
  }
  if (!is.null(main)) {
    p <- p + ggtitle(main)
  }

  # Return "ggplot" object
  p

}
