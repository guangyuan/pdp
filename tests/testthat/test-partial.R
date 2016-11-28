context("Partial dependence functions")

# Data frames used in the following tests
pima2 <- na.omit(pima)
set.seed(101)
df.reg <- boston[sample(nrow(boston), size = 50, replace = FALSE), ]
df.class <- pima2[sample(nrow(pima2), size = 50, replace = FALSE), ]

# Switch to generic response names
df.reg$y <- df.reg$cmedv
df.class$y <- df.class$diabetes
df.reg$cmedv <- NULL
df.class$diabetes <- NULL

test_that("partial works correctly", {

  # Regression
  df.reg.lm <- lm(y ~ .,
                  data = df.reg)

  # Classification
  df.class.glm <- glm(y ~ .,
                      data = df.class,
                      family = binomial)

  # PDPs
  pd1 <- partial(df.reg.lm,
                 pred.var = "rm",
                 grid.resolution = 1,
                 train = df.reg)
  pd2 <- partial(df.reg.lm,
                 pred.var = "rm",
                 pred.grid = 1,
                 check.class = FALSE,
                 train = df.reg)
  pd3 <- partial(df.class.glm,
                 pred.var = "glucose",
                 grid.resolution = 1,
                 train = df.class)

  # Expectations
  expect_is(pd1, "data.frame")
  expect_is(pd2, "data.frame")
  expect_is(partial(df.reg.lm, pred.var = "rm", plot = TRUE, train = df.reg),
            "trellis")
  expect_is(pd3, "data.frame")

})
