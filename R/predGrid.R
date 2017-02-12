#' @keywords internal
trimOutliers <- function(x) {
  out <- grDevices::boxplot.stats(x, do.out = TRUE)$out
  x[!(x %in% out)]  # faster than setdiff in benchmark test
}


#' @keywords internal
predGrid <- function(train, pred.var, grid.resolution = NULL, quantiles = FALSE,
                     probs = 1:9/10, trim.outliers = FALSE, cats = NULL) {

  # train must inherit from one "data.frame", "matrix", or "dgCMatrix"
  if (!inherits(train, c("data.frame", "matrix", "dgCMatrix"))) {
    stop(paste("Training data must be of class \"data.frame\", \"matrix\", ",
               "or \"dgCMatrix\"."))
  }

  # Create a list containing the values of interest for each of the predictor
  # variables listed in pred.var
  pred.val <- lapply(pred.var, function(x) {
    if (is.factor(train[, x])) {
      levels(train[, x])
    } else if (x %in% cats) {
      sort(unique(train[, x]))  # martices cannot contain factors
    } else {
      if (!is.null(grid.resolution) && quantiles) {
        stop("Can only specify one of grid.resolution or quantiles, not both.")
      }
      if (quantiles && trim.outliers) {
        stop("Can only specify one of quantiles or trim.outliers, not both.")
      }
      if (quantiles) {
        stats::quantile(train[, x], probs = probs, na.rm = TRUE, names = FALSE)
      } else {
        if (is.null(grid.resolution)) {
          grid.resolution <- min(length(unique(train[, x])), 51)
        }
        y <- if (trim.outliers) {
          trimOutliers(train[, x])
        } else {
          train[, x]
        }
        seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE),
            length = grid.resolution)
      }
    }
  })

  # Create grid
  pred.grid <- expand.grid(pred.val, KEEP.OUT.ATTRS = FALSE)
  names(pred.grid) <- pred.var
  pred.grid

}


# #' @keywords internal
# predGrid.default <- function(object, pred.var, train, grid.resolution = NULL,
#                              quantiles = FALSE, probs = 1:9/10,
#                              trim.outliers = FALSE) {
#   pred.val <- lapply(pred.var, function(x) {
#     if (is.factor(train[[x]])) {
#       levels(train[[x]])
#     } else {
#       if (!is.null(grid.resolution) && quantiles) {
#         stop("Can only specify one of grid.resolution or quantiles, not both.")
#       }
#       if (quantiles && trim.outliers) {
#         stop("Can only specify one of quantiles or trim.outliers, not both.")
#       }
#       if (quantiles) {
#         stats::quantile(train[[x]], probs = probs, na.rm = TRUE, names = FALSE)
#       } else {
#         if (is.null(grid.resolution)) {
#           grid.resolution <- min(length(unique(train[[x]])), 51)
#         }
#         y <- if (trim.outliers) {
#           trimOutliers(train[[x]])
#         } else {
#           train[[x]]
#         }
#         seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE),
#             length = grid.resolution)
#       }
#     }
#   })
#   pred.grid <- expand.grid(pred.val)
#   names(pred.grid) <- pred.var
#   pred.grid
# }
