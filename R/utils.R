#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Arrange multiple grobs on a page
#'
#' See \code{\link[gridExtra]{grid.arrange}} for more details.
#'
#' @name grid.arrange
#' @rdname grid.arrange
#' @keywords internal
#' @export
#' @importFrom gridExtra grid.arrange
#' @usage grid.arrange(..., newpage = TRUE)
NULL


#' Retrieve the last Trellis Object
#'
#' See \code{\link[lattice]{trellis.last.object}} for more details.
#'
#' @name trellis.last.object
#' @rdname trellis.last.object
#' @keywords internal
#' @export
#' @importFrom lattice  trellis.last.object
#' @usage trellis.last.object(..., prefix)
NULL


#' @keywords internal
copyClasses <- function(x, y) {
  x.names <- names(x)
  y.names <- names(y)
  if (length(setdiff(x.names, y.names)) > 0) {
    stop("Data frame x contains columns not present in data frame y.")
  }
  column.names <- intersect(x.names, y.names)
  for (name in column.names) {
    # Do the classes match?
    if (!identical(class(x[[name]]), class(y[[name]])) ||
        !identical(levels(x[[name]]), levels(y[[name]]))) {
      # Convert to numeric or integer class
      if (is.numeric(y[[name]])) {
        if (is.integer(y[[name]])) {
          x[[name]] <- as.integer(x[[name]])
        } else {
          x[[name]] <- as.numeric(x[[name]])
        }
      }
      # Convert to factor or ordered class
      if (is.factor(y[[name]])) {
        if (is.ordered(y[[name]])) {
          x[[name]] <- as.ordered(x[[name]])
        } else {
          x[[name]] <- as.factor(x[[name]])
        }
        levels(x[[name]]) <- levels(y[[name]])
      }
      # Convert to character
      if (is.character(y[[name]])) {
        x[[name]] <- as.character(x[[name]])
      }
      # Convert to logical
      if (is.logical(y[[name]])) {
        x[[name]] <- as.logical(x[[name]])
      }
    }
  }
  # Sanity check
  stopifnot(all.equal(sapply(x[column.names], class),
                      sapply(y[column.names], class)))
  x  # return x with copied classes
}


#' @keywords internal
avgLogit <- function(x, which.class = 1L) {
  if (is.data.frame(x)) {
    x <- data.matrix(x)
  }
  stopifnot(is.matrix(x))  # x should be a nclass by n probability matrix
  eps <- .Machine$double.eps
  mean(log(ifelse(x[, which.class] > 0, x[, which.class], eps)) -
         rowMeans(log(ifelse(x > 0, x, eps))), na.rm = TRUE)
}


#' @keywords internal
trainCHull <- function(pred.var, pred.grid, train) {
  if (length(pred.var) >= 2 && is.numeric(pred.grid[, 1L]) &&
      is.numeric(pred.grid[, 2L])) {  # if the first two columns are numeric
    if (is.data.frame(train)) {
      train <- data.matrix(train)  # mgcv::in.out requires a matrix
    }
    X <- stats::na.omit(train[, pred.var[1L:2L]])
    Y <- stats::na.omit(data.matrix(pred.grid[, 1L:2L]))
    hpts <- grDevices::chull(X)
    hpts <- c(hpts, hpts[1L])
    keep <- mgcv::in.out(X[hpts, ], Y)
    pred.grid[keep, ]
  } else {
    pred.grid
  }
}
