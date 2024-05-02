#' Objective vs n to find optimal tickets sold
#'
#' @param N Number of seats in the flight
#' @param gamma Probability a plane will be overbooked
#' @param p Probability of a "show"
#'
#' @return A named list of nd, nc, N, gamma, and p as well as a continuous and discrete plot.
#' @export
#' @import ggplot2
#'
#' @examples ntickets(N = 400, gamma = 0.02, p = 0.95)
#'

ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {
  n <- seq(N, floor(N + (0.1 * N)), by = 1)
  discrete <- 1 - gamma - pbinom(q = N, size = n, prob = p)

  cont <- function(n) {
    1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p)))
  }

  continuous <- 1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p)))

  nd <- n[which.min(abs(discrete))]
  nc <- optimize(f = function(x) abs(cont(x)), interval = c(N, floor(N + 0.1 * N)))$minimum

  df <- data.frame(n = n, discrete = discrete)

  plot_discrete <- ggplot(df, aes(x = n, y = discrete)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "blue", size = 3) +
    geom_vline(xintercept = nd, color = "red", linetype = "solid") +
    geom_hline(yintercept = discrete[which(n == nd)], color = "red", linetype = "solid") +
    labs(title = paste("Objective Vs n to find optimal tickets sold \n(", nd, ") gamma =", gamma, " N =", N, "discrete"),
         x = "n", y = "Objective")

  plot_continuous <- ggplot(data.frame(n = c(N, floor(N + 0.1 * N))), aes(x = n)) +
    geom_function(fun = cont, color = "black", linewidth = 1) +
    geom_vline(xintercept = nc, color = "blue", linetype = "solid") +
    geom_hline(yintercept = cont(nc), color = "blue", linetype = "solid") +
    labs(title = paste("Objective Vs n to find optimal tickets sold \n(", nc, ") gamma =", gamma, " N =", N, "continuous"),
         x = "n", y = "Objective")

  print(plot_discrete)
  print(plot_continuous)

  x <- list(nd = nd, nc = nc, N = N,gamma = gamma,p = p)

  cat("Named List:\n")
  print(x)
}

ntickets(N = 400, gamma = 0.02, p = 0.95)
