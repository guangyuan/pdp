# Setup
data(mtcars)
library(gbm)
library(ggplot2)
library(pdp)
library(xgboost)
set.seed(101)
bst <- xgboost(data = as.matrix(mtcars[, -11]), label = mtcars[, 11],
               objective = "count:poisson", nrounds = 50)

# PDP prediction function for XGBoost with Poisson deviance
pfun <- function(object, newdata) {
  mean(exp(predict(object, newdata = as.matrix(newdata))))
}

# One variable
bst %>%
  partial(pred.var = "mpg", pred.fun = pfun, train = mtcars[, -11]) %>%
  autoplot() +
  ylab("Number of carburetors") +
  theme_light()

# One variable
bst %>%
  partial(pred.var = "mpg", inv.link = exp, train = mtcars[, -11],
          recursive = FALSE) %>%
  autoplot() +
  ylab("Number of carburetors") +
  theme_light()

bst %>%
  partial(pred.var = "mpg", train = mtcars[, -11]) %>%
  autoplot() +
  ylab("Number of carburetors") +
  theme_light()

set.seed(101)
mtcars.gbm <- gbm(carb ~ ., data = mtcars, distribution = "poisson",
                  n.trees = 2000, interaction.depth = 3, shrinkage = 0.01,
                  bag.fraction = 1.0, cv.folds = 5)
best.iter <- gbm.perf(mtcars.gbm, method = "cv")
summary(mtcars.gbm, n.trees = best.iter, las = 1)

# Log scale
plot(mtcars.gbm, i.var = "mpg")
partial(mtcars.gbm, pred.var = "mpg", n.trees = best.iter, plot = TRUE)

# Response scale
par(mfrow = c(1, 2))
plot(mtcars.gbm, i.var = "mpg", type = "link")
plot(mtcars.gbm, i.var = "mpg", type = "response")
grid.arrange(
  partial(mtcars.gbm, pred.var = "mpg", recursive = FALSE,
          n.trees = best.iter, plot = TRUE),
  partial(mtcars.gbm, pred.var = "mpg", recursive = FALSE, inv.link = exp,
          n.trees = best.iter, plot = TRUE),
  ncol = 2
)

