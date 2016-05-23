plotPartial <- function(x, ...) {
  UseMethod("plotPartial")
}


plotPartial.partial_1d <- function(x, ...) {
  plot(x, ...)
}


plotPartial.partial_2d <- function(x, contour = TRUE, ...) {
  form <- as.formula(paste("y ~", paste(names(x)[1:2], collapse = "*")))
  if (contour) {
    levelplot(form, data = x, ...)
  } else {
    wireframe(form, data = x, ...)
  }
}
