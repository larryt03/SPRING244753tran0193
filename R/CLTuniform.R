#' CLT Uniform
#'
#' @param n Sample Size
#' @param iter Number of Iterations to generate random samples
#' @param a The lower limit of the uniform distribution
#' @param b The upper limit of the uniform distribution
#' @param x Optional vector of x values for plotting.
#'
#' @return Generate histograms and density plots for the Central Limit Theorem
#' @export
#' @importFrom graphics barplot curve lines polygon
#' @importFrom stats density dnorm dunif runif
#'
#' @examples mycltu(n=1,iter=1000,a=0,b=10)
mycltu <- function(n = 1, iter = 10000, a = 0, b = 10, x = NULL) {
  ## r-random sample from the uniform
  y <- runif(n * iter, a, b)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w <- apply(data, 2, mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param <- hist(w, plot = FALSE)
  ## Since the histogram will be a density plot we will find the max density
  ymax <- max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax <- 1.1 * ymax
  ## Now we can make the histogram
  hist(w, freq = FALSE, ylim = c(0, ymax), main = paste("Histogram of sample mean",
                                                        "\n", "sample size= ", n, sep = ""), xlab = "Sample mean")
  ## add a density curve made from the sample distribution
  lines(density(w), col = "Blue", lwd = 3)  # add a density plot
  ## Add a theoretical normal curve
  if(is.null(x)) {
    curve(dnorm(x, mean = (a + b) / 2, sd = (b - a) / (sqrt(12 * n))), add = TRUE, col = "Red", lty = 2, lwd = 3)  # add a theoretical curve
    ## Add the density from which the samples were taken
    curve(dunif(x, a, b), add = TRUE, lwd = 4)
  } else {
    curve(dnorm(x, mean = (a + b) / 2, sd = (b - a) / (sqrt(12 * n))), add = TRUE, col = "Red", lty = 2, lwd = 3, xlim = c(min(x), max(x)))  # add a theoretical curve
    ## Add the density from which the samples were taken
    curve(dunif(x, a, b), add = TRUE, lwd = 4, xlim = c(min(x), max(x)))
  }
}
