#' @keywords internal
getParDepCls <- function(object, pred.var, pred.grid, which.class, prob, ice,
                         train, progress, parallel, paropts, ...) {

  # Probability scale
  if (prob) {

    # Default Ice curves
    if (ice) {

      # Get predictions
      plyr::adply(pred.grid, .margins = 1, .progress = progress,
                  .parallel = parallel, .paropts = paropts,
                  .fun = function(x) {
                    temp <- train
                    temp[, pred.var] <- x
                    pred <- getIceClsProb(object, newdata = temp,
                                          which.class = which.class, ...)
                    if (is.null(names(pred))) {
                      stats::setNames(pred, paste0("yhat.", 1L:length(pred)))
                    } else {
                      stats::setNames(pred, paste0("yhat.", names(pred)))
                    }
                  }, .id = NULL)

      # Default PDP
    } else {

      # Get predictions
      plyr::adply(pred.grid, .margins = 1, .progress = progress,
                  .parallel = parallel, .paropts = paropts,
                  .fun = function(x) {
                    temp <- train
                    temp[, pred.var] <- x
                    stats::setNames(  # return averaged predicted probabiliy
                      getParClsProb(object, newdata = temp,
                                    which.class = which.class, ...), "yhat"
                    )
                  }, .id = NULL)

    }

    # Centered logit scale
  } else {

    # Default Ice curves
    if (ice) {

      # Get predictions
      plyr::adply(pred.grid, .margins = 1, .progress = progress,
                  .parallel = parallel, .paropts = paropts,
                  .fun = function(x) {
                    temp <- train
                    temp[, pred.var] <- x
                    pred <- getIceClsLogit(object, newdata = temp,
                                           which.class = which.class, ...)
                    if (is.null(names(pred))) {
                      stats::setNames(pred, paste0("yhat.", 1L:length(pred)))
                    } else {
                      stats::setNames(pred, paste0("yhat.", names(pred)))
                    }
                  }, .id = NULL)

      # Default PDP
    } else {

      # Get predictions
      plyr::adply(pred.grid, .margins = 1, .progress = progress,
                  .parallel = parallel, .paropts = paropts,
                  .fun = function(x) {
                    temp <- train
                    temp[, pred.var] <- x
                    stats::setNames(  # return averaged centered logit
                      getParClsLogit(object, newdata = temp,
                                     which.class = which.class, ...), "yhat"
                    )
                  }, .id = NULL)

    }

  }

}
