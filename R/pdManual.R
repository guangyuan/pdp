#' @keywords internal
pdManual <- function(object, pred.var, pred.grid, pred.fun, train, progress,
                     parallel, paropts, ...) {

  # Use plyr::adply, rather than a for loop
  plyr::adply(pred.grid, .margins = 1, .progress = progress,
              .parallel = parallel, .paropts = paropts,
              .fun = function(x) {

                # Copy training data and replace pred.var with constant
                temp <- train
                temp[pred.var] <- x

                # Get prediction(s)
                out <- pred.fun(object, newdata = temp)
                if (length(out) == 1) {
                  stats::setNames(out, "yhat")
                } else {
                  if (is.null(names(out))) {
                    stats::setNames(out, paste0("yhat.", 1L:length(out)))
                  } else {
                    stats::setNames(out, paste0("yhat.", names(out)))
                  }
                }

              })

}
