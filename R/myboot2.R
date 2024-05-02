#' Bootstrap Confidence Intervals
#'
#' @param iter Number of iterations for bootstrap resampling.
#' @param x Numeric vector containing the sample data.
#' @param fun Character string specifying the statistic to compute (e.g., "mean", "median", "var", etc.).
#' @param alpha Confidence level (1 - alpha) for the confidence interval.
#' @param ... Additional arguments to be passed to the histogram plotting function.
#'
#' @return A list containing the computed confidence interval, the statistic used, the original sample,
#' and the bootstrap sample statistics.
#'
#' @export
#' @importFrom stats pbinom pnorm optimize quantile
#' @importFrom graphics hist abline segments text
#' @importFrom grDevices rainbow
#'
#' @examples myboot2(x = 1, fun = "median")
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, ...) {
  n <- length(x)
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)  # Modified nr to nrow and nc to ncol
  xstat <- apply(rs.mat, 2, fun)

  xstat <- xstat[is.finite(xstat)]

  if (length(xstat) == 0) {
    stop("No finite values in xstat")
  }

  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))

  hist(xstat, freq = FALSE, las = 1, main = "Histogram of Bootstrap sample statistics", ...)

  abline(v = mean(xstat), lwd = 3, col = "black")

  segments(ci[1], 0, ci[2], 0, lwd = 4)

  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "red", cex = 3)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "red", cex = 3)

  text(mean(xstat), max(density(xstat)$y) / 2, round(mean(xstat), 2), cex = 3)

  return(list(ci = ci, fun = fun, x = x, xstat = xstat))
}

