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
#' @rdname autoplot
#' @export
autoplot.partial <- function(object, center = TRUE, rug = FALSE, smooth = FALSE,
                             smooth.method = "auto",
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

  if (multi) {
    if (center) {
      object <- object %>%
        dplyr::group_by(yhat.id) %>%
        dplyr::mutate(yhat = yhat - first(yhat))
    }
    p <- ggplot2::ggplot(object, aes(x = object[[1L]], y = yhat)) +
      ggplot2::geom_line(aes(group = yhat.id), ...) +
      ggplot2::stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
      ggplot2::xlab(names(object)[1L]) +
      ggplot2::ylab("yhat")
    if (rug) {
      if (is.null(train)) {
        stop("The training data must be supplied for rug display.")
      } else {
        x.name <- which(names(train) == names(object)[[1L]])
        p <- p + geom_rug(data = train, aes(x = train[[x.name]]),
                          sides = "b",
                          inherit.aes = FALSE)
      }
    }
  } else if (nx == 1L) {
    p <- ggplot2::ggplot(object, aes(object[[1L]], yhat)) +
      ggplot2::geom_line(...) +
      ggplot2::xlab(names(object)[1L]) +
      ggplot2::ylab("yhat")
    if (rug) {
      if (is.null(train)) {
        stop("The training data must be supplied for rug display.")
      } else {
        x.name <- which(names(train) == names(object)[[1L]])
        p <- p + geom_rug(data = train, aes(x = train[[x.name]]),
                          sides = "b",
                          inherit.aes = FALSE)
      }
    }
    if (smooth) {
      p <- p + ggplot2::geom_smooth(method = smooth.method,
                                    formula = smooth.formula,
                                    span = smooth.span, se = FALSE,
                                    method.args = smooth.method.args)
    }
  } else if (nx == 2L) {
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
  } else {
    stop("autoplot.partial does not currently support multiple predictors")
  }

  # Return ggplot object
  p

}
