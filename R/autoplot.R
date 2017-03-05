#' Plotting Partial Dependence Functions
#'
#' Plots partial dependence functions (i.e., marginal effects) using
#' \code{ggplot2} graphics.
#'
#' @param object An object that inherits from the \code{"partial"} class.
#' @param center Logical indicating whether or not to produce centered ICE
#'   curves (c-ICE curves). Only useful when \code{object} represents a set of
#'   ICE curves; see \code{\link[pdp]{partial}} for details. Default is
#'   \code{TRUE}.
#' @param plot.pdp Logical indicating whether or not to plot the partial
#'   dependence function on top of the ICE curves. Default is \code{TRUE}.
#' @param pdp.color Character string specifying the color to use for the partial
#'   dependence function when \code{plot.pdp = TRUE}. Default is \code{"red"}.
#' @param pdp.size The line width, a positive number, to use for the partial
#'   dependence function when \code{plot.pdp = TRUE}. Default is \code{1}.
#' @param smooth Logical indicating whether or not to overlay a LOESS smooth.
#'   Default is \code{FALSE}.
#' @param smooth.method Character string specifying the smoothing method
#'   (function) to use (e.g., \code{"auto"}, \code{"lm"}, \code{"glm"},
#'   \code{"gam"}, \code{"loess"}, or \code{"rlm"}). Default is \code{"auto"}.
#'   See \code{\link[ggplot2]{geom_smooth}} for details.
#' @param smooth.formula Formula to use in smoothing function (e.g.,
#'   \code{y ~ x}, \code{y ~ poly(x, 2)}, or \code{y ~ log(x)}).
#' @param smooth.span Controls the amount of smoothing for the default loess
#'   smoother. Smaller numbers produce wigglier lines, larger numbers produce
#'   smoother lines. Default is \code{0.75}.
#' @param smooth.method.args List containing additional arguments to be passed
#'   on to the modelling function defined by \code{smooth.method}.
#' @param contour Logical indicating whether or not to add contour lines to the
#'   level plot. Only used when \code{levelplot = TRUE}. Default is
#'   \code{FALSE}.
#' @param contour.color Character string specifying the color to use for the
#'   contour lines when \code{contour = TRUE}. Default is \code{"white"}.
#' @param palette If a string, will use that named palette. If a number, will
#'   index into the list of palettes of appropriate type. Default is
#'   \code{"Spectral"}.
#' @param train Data frame containing the original training data. Only
#'   required if \code{rug = TRUE} or \code{chull = TRUE}.
#' @param xlab Charater string specifying the text for the x-axis label.
#' @param ylab Charater string specifying the text for the y-axis label.
#' @param legend.title Charater string specifying the text for the legend title.
#'   Default is \code{"yhat"}.
#' @param ... Additional optional arguments to be passed onto \code{geom_line}.
#'
#' @return A \code{"ggplot"} object.
#'
#' @importFrom ggplot2 aes autoplot geom_contour geom_line geom_point geom_rug
#' @importFrom ggplot2 geom_smooth geom_tile scale_fill_distiller stat_summary
#' @importFrom ggplot2 theme_bw xlab ylab
#'
#' @rdname autoplot
#' @export
autoplot.partial <- function(object, center = TRUE, plot.pdp = TRUE,
                             pdp.color = "red", pdp.size = 1, rug = FALSE,
                             smooth = FALSE, smooth.method = "auto",
                             smooth.formula = y ~ x, smooth.span = 0.75,
                             smooth.method.args = list(), contour = FALSE,
                             contour.color = "white", palette = "Spectral",
                             train = NULL,
                             xlab = NULL, ylab = NULL, legend.title = NULL,
                             ...) {

  # Determine of x contains multiple PDPs
  multi <- if ("yhat.id" %in% names(object)) {
    TRUE
  } else {
    FALSE
  }

  # Determine number of variables to plot
  nx <- if (multi) {
    ncol(object) - 2  # don't count yhat or yhat.id
  } else {
    ncol(object) - 1  # don't count yhat
  }

  # Multiple curves
  if (multi) {

    p <- ggPDPMulti(object, center = center, plot.pdp = plot.pdp,
                    pdp.color = pdp.color, pdp.size = pdp.size, rug = rug,
                    train = train, xlab = xlab, ylab = ylab, ...)

  # One predictor
  } else if (nx == 1L) {

    p <- if (is.factor(object[[1L]])) {
      ggPDPFactor(object, xlab = xlab, ylab = ylab, ...)
    } else {
      ggPDPNumeric(object, rug = rug, smooth = smooth,
                   smooth.method = smooth.method,
                   smooth.formula = smooth.formula,
                   smooth.span = smooth.span,
                   smooth.method.args = smooth.method.args, train = train,
                   xlab = xlab, ylab = ylab, ...)
    }

  # Two predictors
  } else if (nx == 2L) {

    p <- if (is.factor(object[[1L]]) && is.factor(object[[2L]])) {
      ggPDPFactorFactor(object, xlab = xlab, ylab = ylab, ...)
    } else if (is.factor(object[[1L]]) && !is.factor(object[[2L]])) {
      ggPDPFactorNumeric(object, smooth = smooth,
                         smooth.method = smooth.method,
                         smooth.formula = smooth.formula,
                         smooth.span = smooth.span,
                         smooth.method.args = smooth.method.args, train = train,
                         xlab = xlab, ylab = ylab, ...)
    } else if (!is.factor(object[[1L]]) && is.factor(object[[2L]])) {
      ggPDPNumericFactor(object, smooth = smooth,
                         smooth.method = smooth.method,
                         smooth.formula = smooth.formula,
                         smooth.span = smooth.span,
                         smooth.method.args = smooth.method.args, train = train,
                         xlab = xlab, ylab = ylab, ...)
    } else {
      ggPDPNumericNumeric(object, contour = contour,
                          contour.color = contour.color, palette = palette,
                          train = train, xlab = xlab, ylab = ylab,
                          legend.title = legend.title, ...)
    }

    p <- ggplot(object, aes(x = object[[1L]], y = object[[2L]], z = yhat,
                            fill = yhat)) +
      ggplot2::geom_tile()
    if (contour) {
      p <- p + ggplot2::geom_contour(color = contour.color)
    }
    if (is.null(xlab)) {
      p <- p + ggplot2::xlab(names(object)[1L])
    } else {
      p <- p + ggplot2::xlab(xlab)
    }
    if (is.null(ylab)) {
      p <- p + ggplot2::ylab(names(object)[2L])
    } else {
      p <- p + ggplot2::ylab(ylab)
    }
    if (is.null(legend.title)) {
      p <- p + ggplot2::scale_fill_distiller(name = "yhat", palette = palette)
    } else {
      p <- p + ggplot2::scale_fill_distiller(name = legend.title,
                                             palette = palette)
    }
    p <- p + ggplot2::theme_bw()

  # More then two predictors
  } else {
    stop("autoplot does not currently support more than two predictors")
  }

  # Return ggplot object
  p

}


#' @keywords internal
ggPDPMulti <- function(object, center, plot.pdp, pdp.color, pdp.size, rug, train,
                       xlab, ylab, ...) {
  if (is.factor(object[[1L]])) {
    p <- ggplot(object, aes(object[[1L]], yhat, group)) +
      geom_point(aes(group = yhat.id), ...)
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
  } else {
    if (center) {
      object <- object %>%
        dplyr::group_by(yhat.id) %>%
        dplyr::mutate(yhat = yhat - first(yhat))
    }
    p <- ggplot(object, aes(x = object[[1L]], y = yhat)) +
      geom_line(aes(group = yhat.id), ...) +
      xlab(names(object)[1L]) +
      ylab("yhat")
    if (plot.pdp) {
      p <- p + stat_summary(fun.y = mean, geom = "line", col = pdp.color,
                            size = pdp.size)
    }
    if (rug) {
      if (is.null(train)) {
        stop("The training data must be supplied for rug display.")
      } else {
        x.name <- which(names(train) == names(object)[[1L]])
        x.rug <- data.frame(as.numeric(
          stats::quantile(train[, x.name, drop = TRUE], probs = 0:10/10,
                          na.rm = TRUE)))
        p <- p + geom_rug(data = x.rug, aes(x = x.rug[, 1L]),
                          sides = "b", inherit.aes = FALSE)
      }
    }
    if (is.null(xlab)) {
      p <- p + xlab(names(object)[1L])
    } else {
      p <- p + xlab(xlab)
    }
    if (is.null(ylab)) {
      p <- p + ggplot2::ylab("yhat")
    } else {
      p <- p + ggplot2::ylab(ylab)
    }
  }
  p
}


#' @keywords internal
ggPDPFactor <- function(object, xlab, ylab, ...) {
  p <- ggplot(object, aes(object[[1L]], yhat)) +
    geom_point(...)
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
  p
}


#' @keywords internal
ggPDPNumeric <- function(object, rug, smooth, smooth.method, smooth.formula,
                         smooth.span, smooth.method.args, train, xlab, ylab,
                         ...) {
  p <- ggplot(object, aes(object[[1L]], yhat)) +
    geom_line(...)
  if (rug) {
    if (is.null(train)) {
      stop("The training data must be supplied for rug display.")
    } else {
      x.name <- which(names(train) == names(object)[[1L]])
      x.rug <- data.frame(as.numeric(
        stats::quantile(train[, x.name, drop = TRUE], probs = 0:10/10,
                        na.rm = TRUE)))
      p <- p + geom_rug(data = x.rug, aes(x = x.rug[, 1L]), sides = "b",
                        inherit.aes = FALSE)
    }
  }
  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = smooth.method,
                                  formula = smooth.formula,
                                  span = smooth.span, se = FALSE,
                                  method.args = smooth.method.args)
  }
  if (is.null(xlab)) {
    p <- p + xlab(names(object)[1L])
  } else {
    p <- p + xlab(xlab)
  }
  if (is.null(ylab)) {
    p <- p + ggplot2::ylab("yhat")
  } else {
    p <- p + ggplot2::ylab(ylab)
  }
  p
}


#' @keywords internal
ggPDPFactorFactor <- function(object, xlab, ylab, ...) {
  p <- ggplot(object, aes(object[[1L]], yhat)) +
    geom_point(...) +
    facet_wrap(~ object[[2L]])
  if (is.null(xlab)) {
    p <- p + xlab(names(object)[1L])
  } else {
    p <- p + xlab(xlab)
  }
  if (is.null(ylab)) {
    p <- p + ggplot2::ylab("yhat")
  } else {
    p <- p + ggplot2::ylab(ylab)
  }
  p
}


#' @keywords internal
ggPDPNumericFactor <- function(object, smooth, smooth.method,
                               smooth.formula, smooth.span, smooth.method.args,
                               train, xlab, ylab, ...) {
  p <- ggplot(object, aes(object[[1L]], yhat)) +
    geom_line(...) +
    facet_wrap(~ object[[2L]])
  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = smooth.method,
                                  formula = smooth.formula,
                                  span = smooth.span, se = FALSE,
                                  method.args = smooth.method.args)
  }
  if (is.null(xlab)) {
    p <- p + xlab(names(object)[1L])
  } else {
    p <- p + xlab(xlab)
  }
  if (is.null(ylab)) {
    p <- p + ggplot2::ylab("yhat")
  } else {
    p <- p + ggplot2::ylab(ylab)
  }
  p
}


#' @keywords internal
ggPDPFactorNumeric <- function(object, smooth, smooth.method,
                               smooth.formula, smooth.span, smooth.method.args,
                               train, xlab, ylab, ...) {
  p <- ggplot(object, aes(object[[2L]], yhat)) +
    geom_line(...) +
    facet_wrap(~ object[[1L]])
  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = smooth.method,
                                  formula = smooth.formula,
                                  span = smooth.span, se = FALSE,
                                  method.args = smooth.method.args)
  }
  if (is.null(xlab)) {
    p <- p + xlab(names(object)[2L])
  } else {
    p <- p + xlab(xlab)
  }
  if (is.null(ylab)) {
    p <- p + ggplot2::ylab("yhat")
  } else {
    p <- p + ggplot2::ylab(ylab)
  }
  p
}


#' @keywords internal
ggPDPNumericNumeric <- function(object, contour, contour.color, palette,
                                train, xlab, ylab, legend.title, ...) {
  p <- ggplot(object, aes(x = object[[1L]], y = object[[2L]], z = yhat,
                          fill = yhat)) +
    geom_tile()
  if (contour) {
    p <- p + geom_contour(color = contour.color)
  }
  if (is.null(xlab)) {
    p <- p + xlab(names(object)[1L])
  } else {
    p <- p + xlab(xlab)
  }
  if (is.null(ylab)) {
    p <- p + ylab(names(object)[2L])
  } else {
    p <- p + ylab(ylab)
  }
  if (is.null(legend.title)) {
    p <- p + scale_fill_distiller(name = "yhat", palette = palette)
  } else {
    p <- p + scale_fill_distiller(name = legend.title,
                                           palette = palette)
  }
  p <- p + theme_bw()
}
