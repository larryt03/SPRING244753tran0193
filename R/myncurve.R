#' Plot Normal Distribution Curve with Shaded Area
#'
#' @param mu The mean of the normal distribution.
#' @param sigma The standard deviation of the normal distribution.
#' @param a The upper limit of integration
#'
#' @return A graph of the normal distribution
#' @export
#'
#' @examples myncurve(mu = 0, sigma = 1, a = 1)
myncurve <- function(mu, sigma, a) {
  x <- seq(mu - 3 * sigma, mu + 3 * sigma, length.out = 1000)  # Define x values for the curve
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma))

  # Shade area under the curve from -∞ to x=a
  x_values <- seq(mu - 3 * sigma, a, length.out = 1000)
  y_values <- dnorm(x_values, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, x_values, a), c(0, y_values, 0), col = "blue", border = NA)

  # Calculate the area (probability) P(X <= a)
  prob <- pnorm(a, mean = mu, sd = sigma)

  # Return results as a list
  result <- list(mu = mu, sigma = sigma, area = prob)
  return(result)
}
