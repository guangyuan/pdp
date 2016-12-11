#-------------------------------------------------------------------------------
# Slow tests for the pdp package
#-------------------------------------------------------------------------------


################################################################################
# Setup
################################################################################

# Load required packages
library(caret)
library(pdp)

# Load data
data(boston)  # regression
data(pima)  # classification


################################################################################
# randomForest package
################################################################################

# Load required package
library(randomForest)

# Regression
set.seed(101)
boston.randomForest <- randomForest(cmedv ~ ., data = boston, importance = TRUE)

# Partial dependence of rm and lstat on cmedv
pdp1 <- partial(boston.randomForest, pred.var = "rm", plot = TRUE, 
                rug = TRUE, progress = "text")
pdp2 <- partial(boston.randomForest, pred.var = "lstat", plot = TRUE, 
                rug = TRUE, progress = "text")
pdp3 <- partial(boston.randomForest, pred.var = "lon", plot = TRUE, 
                rug = TRUE, progress = "text")
pdp4 <- partial(boston.randomForest, pred.var = c("rm", "lstat"), plot = TRUE, 
                rug = TRUE, chull = TRUE, progress = "text")
multiplot(pdp1, pdp2, pdp3, pdp4, ncol = 2)



ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = TRUE)
set.seed(101)
boston.mod <- train(x = data.matrix(subset(boston, select = -cmedv)),
                    y = boston$cmedv,
                    method = "svmRadial",
                    metric = "Rsquared",
                    trControl = ctrl,
                    tuneLength = 10)
plot(boston.mod)
plot(varImp(boston.mod))
partial(boston.mod, pred.var = "rm", plot = TRUE)

topPredictors <- function(object, n = 1L, ...) {
  UseMethod("topPredictors")
}
topPredictors.train <- function(object, n = 1L, ...) {
  imp <- varImp(object)$importance
  imp <- imp[order(imp$Overall, decreasing = TRUE), , drop = FALSE]
  rownames(imp)[seq_len(n)]
}
topPredictors(boston.mod, 2)
pdp.top <- boston.mod %>%
  partial(topPredictors(boston.mod, n = 2), plot = TRUE, 
          chull = TRUE, progress = "text") %>%
  print()

set.seed(101)
mushroom.rf <- train(x = subset(mushroom, select = -Edibility),
                     y = mushroom$Edibility,
                     method = "rf",
                     trControl = trainControl(method = "cv", number = 10, 
                                              verboseIter = TRUE),
                     tuneLength = 22)

pd.odor <- partial(mushroom.rf, pred.var = "odor")
p1 <- ggplot(varImp(mushroom.rf), color = "black") +
  theme_light() +
  xlab("") +
  theme_bw()
p2 <- ggplot(pd.odor, aes(x = odor, y = y)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  xlab("Odor") +
  ylab("Log odds of being edible") +
  coord_flip() +
  theme_bw()
multiplot(p1, p2, ncol = 2)
