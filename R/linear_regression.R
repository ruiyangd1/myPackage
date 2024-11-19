#' Linear Regression Model
#'
#' Fits a simple linear regression model.
#'
#' @param x Numeric vector (independent variable).
#' @param y Numeric vector (dependent variable).
#' @return An object of class "lm".
#' @examples
#' x <- 1:10
#' y <- 1:10 + rnorm(10)
#' model <- linear_regression(x, y)
#' summary(model)
#' @export
linear_regression <- function(x, y) {
  model <- lm(y ~ x)
  return(model)
}

