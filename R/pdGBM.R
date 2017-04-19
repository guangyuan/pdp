#' @keywords internal
#' @useDynLib pdp, .registration = TRUE
#' @importFrom Rcpp sourceCpp
pdGBM <- function(object, pred.var, pred.grid, which.class, prob, ...) {

  # Extract number of trees
  dots <- list(...)
  if ("n.trees" %in% names(dots)) {
    n.trees <- dots$n.trees
    if (!is.numeric(n.trees) || length(n.trees) != 1) {
      stop("\"n.trees\" must be a single integer")
    }
  } else {
    stop("argument \"n.trees\" is missing, with no default")
  }

  # Extract number of response classes for gbm_plot
  if (is.null(object$num.classes)) {
    object$num.classes <- 1
  }

  # Partial dependence values
  y <- .Call("PartialGBM",
             X = as.double(data.matrix(pred.grid)),
             cRows = as.integer(nrow(pred.grid)),
             cCols = as.integer(ncol(pred.grid)),
             n.class = as.integer(object$num.classes),
             pred.var = as.integer(match(pred.var, object$var.names) - 1),
             n.trees = as.integer(n.trees),
             initF = as.double(object$initF),
             trees = object$trees,
             c.splits = object$c.splits,
             var.type = as.integer(object$var.type),
             PACKAGE = "pdp")

  # Data frame of predictor values (pd values will be added to this)
  pd.df <- pred.grid

  # Transform/rescale predicted values
  if (object$distribution$name == "multinomial") {
    if (prob) {
      pd.df$yhat <- mean(matrix(y, ncol = object$num.classes)[, which.class],
                         na.rm = TRUE)
    } else {
      pd.df$yhat <- mean(multiClassLogit(matrix(y, ncol = object$num.classes),
                                         which.class = which.class),
                         na.rm = TRUE)
    }
  } else if (object$distribution$name %in% c("bernoulli", "pairwise")) {
    pr <- boot::inv.logit(y)
    pr <- cbind(pr, 1 - pr)
    if (prob) {
      pd.df$yhat <- pr[, which.class]
    } else {
      eps <- .Machine$double.eps
      pd.df$yhat <- log(ifelse(pr[, which.class] > 0, pr[, which.class], eps)) -
        rowMeans(log(ifelse(pr > 0, pr, eps)))
    } # pd.df$yhat <- boot::logit(cbind(pr, 1 - pr)[, which.class])
  } else {
    pd.df$yhat <- y
  }

  # Return data frame of predictor and partial dependence values
  pd.df

}
