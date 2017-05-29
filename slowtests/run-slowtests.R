# Function to run tests for a fitted model object using the Boston housing data
bostonHousingTest <- function(object) {

  # Partial dependence plot for lstat
  pd1 <- partial(object, pred.var = "lstat", train = boston)

  # Partial dependence plot for lstat and rm
  pd2 <- partial(object, pred.var = c("lstat", "rm"), chull = TRUE,
                 train = boston)

  # Individual conditional expectation curves for age
  pd3 <- partial(object, pred.var = "age", ice = TRUE, train = boston)

  # Centered individual conditional expectation curves for lstat
  pd4 <- partial(object, pred.var = "age", ice = TRUE, center = TRUE,
                 train = boston)

  # Display plots
  grid.arrange(
    plotPartial(pd1, rug = TRUE, train = boston, main = "PDP for lstat"),
    plotPartial(pd2, main = "PDP for lstat and rm"),
    plotPartial(pd3, main = "ICE curves for age", alpha = 0.5),
    plotPartial(pd4, main = "c-ICE curves for age", alpha = 0.5),
    ncol = 2,
    top = textGrob(deparse(substitute(object)),
                   gp = gpar(fontsize = 20, font = 3))
  )

}


# Function to run tests for a fitted model object using the Boston housing data
pimaIndiansTest <- function(object) {

  # Partial dependence plot for lstat
  pd1 <- partial(object, pred.var = "glucose", train = pima)

  # Partial dependence plot for lstat and rm
  pd2 <- partial(object, pred.var = c("glucose", "age"), chull = TRUE,
                 train = pima)

  # Partial dependence plot for lstat
  pd3 <- partial(object, pred.var = "glucose", prob = TRUE, train = pima)

  # Partial dependence plot for lstat and rm
  pd4 <- partial(object, pred.var = c("glucose", "age"), prob = TRUE,
                 chull = TRUE, train = pima)

  # Individual conditional expectation curves for age
  pd5 <- partial(object, pred.var = "glucose", ice = TRUE, train = pima)

  # Centered individual conditional expectation curves for lstat
  pd6 <- partial(object, pred.var = "glucose", ice = TRUE, center = TRUE,
                 train = pima)

  # Individual conditional expectation curves for age
  pd7 <- partial(object, pred.var = "glucose", ice = TRUE, prob = TRUE,
                 train = pima)

  # Centered individual conditional expectation curves for lstat
  pd8 <- partial(object, pred.var = "glucose", ice = TRUE, center = TRUE,
                 prob = TRUE, train = pima)

  # Display plots
  grid.arrange(
    plotPartial(pd1, rug = TRUE, train = pima, main = "PDP for lstat"),
    plotPartial(pd2, main = "PDP for glucose and age"),
    plotPartial(pd3, rug = TRUE, train = pima,
                main = "PDP for lstat (probability)"),
    plotPartial(pd4, main = "PDP for glucose and age (probability)"),
    plotPartial(pd5, rug = TRUE, train = pima,
                main = "ICE curves for glucose", alpha = 0.5),
    plotPartial(pd6, rug = TRUE, train = pima,
                main = "c-ICE curves for glucose", alpha = 0.5),
    plotPartial(pd7, rug = TRUE, train = pima, plot.pdp = TRUE,
                main = "ICE curves for glucose (probability)", alpha = 0.5),
    plotPartial(pd8, rug = TRUE, train = pima,
                main = "c-ICE curves for glucose (probability)", alpha = 0.5),
    ncol = 4,
    top = textGrob(deparse(substitute(object)),
                   gp = gpar(fontsize = 20, font = 3))
  )

}


data(boston)
data(pima)

set.seed(101)
boston.rf <- randomForest(cmedv ~ ., data = boston)
pima.rf <- randomForest(diabetes ~ ., data = pima, na.action = na.omit)

bostonHousingTest(boston.rf)
pimaIndiansTest(pima.rf)
