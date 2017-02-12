# Load required packages
library(Matrix)
library(pdp)
library(xgboost)

# Load the data
data(pima)

# Set up training data
X <- subset(pima, select = -diabetes)
X.matrix <- data.matrix(X)
X.dgCMatrix <- as(data.matrix(X), "dgCMatrix")
y <- ifelse(pima$diabetes == "pos", 1, 0)

# List of parameters for XGBoost
plist <- list(
  max_depth = 3, 
  eta = 0.01, 
  objective = "binary:logistic", 
  eval_metric = "auc"
)

# Fit an XGBoost model with trainind data stored as a "matrix"
set.seed(101)
bst.matrix <- xgboost(data = X.matrix, label = y, params = plist, nrounds = 100)

# Fit an XGBoost model with trainind data stored as a "dgCMatrix"
set.seed(101)
bst.dgCMatrix <- xgboost(data = X.dgCMatrix, label = y, params = plist,
                         nrounds = 100)

# Fit an XGBoost model with trainind data stored as an "xgb.DMatrix"
set.seed(101)
bst.xgb.DMatrix <- xgboost(data = xgb.DMatrix(data.matrix(X), label = y), 
                           params = plist, nrounds = 100)

# Try all nine combnations
par(mfrow = c(3, 3))
plot(partial(bst.matrix, pred.var = "glucose", train = X))
plot(partial(bst.matrix, pred.var = "glucose", train = X.matrix))
plot(partial(bst.matrix, pred.var = "glucose", train = X.dgCMatrix))
plot(partial(bst.dgCMatrix, pred.var = "glucose", train = X))
plot(partial(bst.dgCMatrix, pred.var = "glucose", train = X.matrix))
plot(partial(bst.dgCMatrix, pred.var = "glucose", train = X.dgCMatrix))
plot(partial(bst.xgb.DMatrix, pred.var = "glucose", train = X))
plot(partial(bst.xgb.DMatrix, pred.var = "glucose", train = X.matrix))
plot(partial(bst.xgb.DMatrix, pred.var = "glucose", train = X.dgCMatrix))

