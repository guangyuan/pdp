#' Arrange Multiple grobs on a Page
#'
#' Set up a gtable layout to place multiple grobs on a page.
#'
#' @param ... grobs, gtables, ggplot or trellis objects.
#' @param nrow Integer specifying the number of rows of plots.
#' @param ncol Integer specifying the number of columns of plots.
#' @return AN object of class \code{c("gtable", "gTree", "grob", "gDesc")}.
#' @details \code{multiplot} is just a simple wrapper around 
#'   \code{\link[gridExtra]{grid.arrange}}. For details see the documentation 
#'   for that function.
#' @export
#' @examples 
#' \dontrun{
#' # Fit a random forest to the boston housing data
#' library(randomForest)
#' data (boston)  # load the boston housing data
#' set.seed(101)  # for reproducibility
#' boston.rf <- randomForest(cmedv ~ ., data = boston)
#' pdp.rm <- partial(boston.rf, pred.var = "rm", plot = TRUE)
#' pdp.lstat <- partial(boston.rf, pred.var = "lstat", plot = TRUE)
#' pdp.rm.lstat <- partial(boston.rf, pred.var = c("rm", "lstat"), plot = TRUE)
#' multiplot(pdp.rm, pdp.lstat, pdp.rm.lstat, ncol = 3)
#' }
multiplot <- function(..., nrow = NULL, ncol = NULL) {
  gridExtra::grid.arrange(..., nrow = nrow, ncol = ncol)
}
