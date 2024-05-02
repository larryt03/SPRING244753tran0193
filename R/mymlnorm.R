#' Max Likelihood with Two Parameters
#'
#' @param x A vector containing the sample data.
#' @param mu A vector of possible mean values to evaluate.
#' @param sig A vector of possible standard deviation values to evaluate.
#' @param ... Additional arguments to be passed to the plotting function.
#'
#' @return A list containing the sample data (x), the coordinates of the maximum likelihood estimate (coord), and the maximum likelihood value (maxl).
#' @export
#'
#' @importFrom graphics contour
#' @importFrom stats sd
#'
#' @examples mymlnorm(x=c(5,7,7,8,10),mu=seq(5,10,length=1000),sig=seq(0.1,4,length=1000),lwd=2,labcex=1)
#'
mymlnorm <- function(x, mu, sig, ...) {
  nmu <- length(mu) # number of values in mu
  nsig <- length(sig)
  n <- length(x) # sample size
  zz <- c()    ## initialize a new vector
  lfun <- function(x, m, p) log(dnorm(x, mean = m, sd = p))   # log lik for normal

  for (j in 1:nsig) {
    z <- outer(x, mu, lfun, p = sig[j]) # z a matrix
    y <- apply(z, 2, sum)
    zz <- cbind(zz, y)
  }

  maxl <- max(exp(zz))
  coord <- which(exp(zz) == maxl, arr.ind = TRUE)

  maxlsig <- apply(zz, 1, max)

  contour(mu, sig, exp(zz), las = 3, xlab = expression(mu), ylab = expression(sigma), axes = TRUE,
          main = expression(paste("L(", mu, ",", sigma, ")", sep = "")), ...)

  mlx <- round(mean(x), 2)  # theoretical
  mly <- round(sqrt((n - 1) / n) * sd(x), 2)

  abline(v = mean(x), lwd = 2, col = "Green")
  abline(h = sqrt((n - 1) / n) * sd(x), lwd = 2, col = "Red")

  muest <- mu[coord[1]]
  sigest <- sig[coord[2]]

  abline(v = muest, h = sigest)

  return(list(x = x, coord = coord, maxl = maxl))
}
