#' Linear Regression model
#'
#' This function fits a linear regression model using mathematical operations.
#'
#' @param x A numeric vector (independent variable).
#' @param y A numeric vector (dependent variable).
#' @return A list containing estimates, variance, standard error, t-statistics, and p-values.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 6, 8, 10) + rnorm(5)
#' linear_regression(x, y)
#' @export
#'
#'
linear_regression <- function(x, y) {
  # Ensure x and y are numeric
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric.")
  }


  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }


  X <- cbind(1, x)  # Intercept + predictors

  # Calculate regression coefficients: (X'X)^-1 * X'y
  XtX <- t(X) %*% X
  XtX_inv <- solve(XtX)
  XtY <- t(X) %*% y
  beta <- XtX_inv %*% XtY

  # Compute residuals and predicted values
  y_hat <- X %*% beta
  residuals <- y - y_hat

  # Compute residual variance (sigma^2)
  n <- nrow(X)
  p <- ncol(X)
  sigma_sq <- sum(residuals^2) / (n - p)

  # Variance-Covariance Matrix
  var_cov_matrix <- sigma_sq * XtX_inv

  # Standard Errors of Coefficients
  se <- sqrt(diag(var_cov_matrix))

  # t-statistics
  t_stat <- beta / se

  # p-values (two-tailed test)
  p_val <- 2 * pt(-abs(t_stat), df = n - p)

  # Return results as a list
  return(list(
    estimates = as.vector(beta),
    variance = diag(var_cov_matrix),
    se = as.vector(se),
    t_stat = as.vector(t_stat),
    p_val = as.vector(p_val),
    residuals = residuals,
    fitted_values = y_hat
  ))
}
