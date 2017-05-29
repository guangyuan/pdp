# Data frames used in the following tests
pima2 <- na.omit(pima)
set.seed(101)
df.reg <- boston[sample(nrow(boston), size = 40, replace = FALSE), ]
df.class <- pima2[sample(nrow(pima2), size = 40, replace = FALSE), ]

# Switch to generic response names
df.reg$y <- df.reg$cmedv
df.class$y <- df.class$diabetes
df.reg$cmedv <- NULL
df.class$diabetes <- NULL
