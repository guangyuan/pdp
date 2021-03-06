% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topPredictors.R
\name{topPredictors}
\alias{topPredictors}
\alias{topPredictors.default}
\alias{topPredictors.train}
\title{Extract Most "Important" Predictors (Experimental)}
\usage{
topPredictors(object, n = 1L, ...)

\method{topPredictors}{default}(object, n = 1L, ...)

\method{topPredictors}{train}(object, n = 1L, ...)
}
\arguments{
\item{object}{A fitted model object of appropriate class (e.g., \code{"gbm"},
\code{"lm"}, \code{"randomForest"}, etc.).}

\item{n}{Integer specifying the number of predictors to return. Default is
\code{1} meaning return the single most important predictor.}

\item{...}{Additional optional arguments to be passed onto
\code{\link[caret]{varImp}}.}
}
\description{
Extract the most "important" predictors for regression and classification
models.
}
\details{
This function uses the generic function \code{\link[caret]{varImp}} to
calculate variable importance scores for each predictor. After that, they are
sorted at the names of the \code{n} highest scoring predictors are returned.
}
\examples{
\dontrun{
#
# Regression example (requires randomForest package to run)
#

Load required packages
library(ggplot2)
library(randomForest)

# Fit a random forest to the mtcars dataset
data(mtcars, package = "datasets")
set.seed(101)
mtcars.rf <- randomForest(mpg ~ ., data = mtcars, mtry = 5, importance = TRUE)

# Topfour predictors
top4 <- topPredictors(mtcars.rf, n = 4)

# Construct partial dependence functions for top four predictors
pd <- NULL
for (i in top4) {
  tmp <- partial(mtcars.rf, pred.var = i)
  names(tmp) <- c("x", "y")
  pd <- rbind(pd,  cbind(tmp, predictor = i))
}

# Display partial dependence functions
ggplot(pd, aes(x, y)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw() +
  ylab("mpg")

}
}
