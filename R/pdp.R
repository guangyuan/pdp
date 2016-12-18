#' pdp: A general framework for constructing partial dependence (i.e., marginal
#' effect) plots from various types machine learning models in R.
#'
#' Partial dependence plots (PDPs) help visualize the relationship between a
#' subset of the features (typically 1-3) and the response while accounting for
#' the average effect of the other predictors in the model. They are
#' particularly effective with black box models like random forests and support
#' vector machines.
#'
#' The development version can be found on GitHub: https://github.com/bgreenwell/pdp.
#' As of right now, \code{pdp} only exports two functions:
#' \itemize{
#'   \item{\code{partial}} --- Compute partial dependence functions (i.e., marginal effects) for various model fitting objects.
#'   \item{\code{plotPartial}} --- Plot partial dependence functions (i.e., marginal effects) using lattice graphics.
#' }
#'
#' @docType package
#' @name pdp
NULL
