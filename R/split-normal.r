#' Estimate split normal
#'
#' Estimate a split normal from mode, p10 and p90
#' @param mode most likely value
#' @param p10  10th percentile value (p10 < mode)
#' @param p90  90th percentile value (mode < p90)
#' @param tol  positive number convergence tolerance
#' @returns list with split normal parameters (mode, sd1, sd2)

est_spnorm_from_p10p90 <- function(mode, p10, p90, tol = 1e-10) {
  # check supplied arguments are valid
  stopifnot(is.finite(mode), is.finite(p10), is.finite(p90))

  if (!(p10 < mode && mode < p90)) {
    stop("Function requires p10 < mode < p90")
  }

  # ensure numerical stability
  eps <- 1e-6 # epsilon (arbitrarily small positive value)

  # alpha must satisfy: 0.1/(2*alpha) ∈ (0, 0.5) and RHS ∈ (0.5, 1)
  lower <- max(0.1 + eps, eps)
  upper <- min(0.90 - eps, 1 - eps)

  # fn to estimate alpha (share of total scale allocated to the left side)
  f <- function(alpha) {
    # implied z-scores
    z_l <- stats::qnorm(0.1 / (2 * alpha)) # < 0
    z_r <- stats::qnorm(0.5 + (0.90 - alpha) / (2 * (1 - alpha))) # > 0

    # convert to sigmas
    sigma_l <- (mode - p10) / -z_l
    sigma_r <- (p90 - mode) / z_r

    # fixed-point residual: implied alpha_hat - alpha
    alpha_hat <- sigma_l / (sigma_l + sigma_r)
    alpha_hat - alpha
  }

  # bisection for alpha
  r <- stats::uniroot(f, c(lower, upper), tol = tol)
  alpha <- r$root

  # return split normal parameters
  z_l <- stats::qnorm(0.1 / (2 * alpha))
  z_r <- stats::qnorm(0.5 + (0.90 - alpha) / (2 * (1 - alpha)))
  sigma_l <- (mode - p10) / -z_l
  sigma_r <- (p90 - mode) / z_r

  list(
    mu = mode,
    sigma_l = sigma_l,
    sigma_r = sigma_r,
    alpha = alpha # Do I need to save this?
  )
}

#' Calculate PDF for split normal
#' @description Generates and then plots a split normal from
#' the mode, and two sd (left and right)
#' @param x sample grid
#' @param mu mode 
#' @param sigma_l sigma for left distribution
#' @param sigma_r sigma for right distribution
split_pdf <- function(x, mu = 0, sigma_l = 1, sigma_r = 1) {
  c <- sqrt(2 / pi) / (sigma_l + sigma_r)
  ifelse(
    x < mu,
    c * exp(- (x - mu)^2 / (2 * sigma_l^2)),
    c * exp(- (x - mu)^2 / (2 * sigma_r^2))
  )
}

#' Plot split normal
#' Generates and then plots a split normal from
#' the mode, and two sd (left and right)
#' @param fit object returned from est_spnorm_from_p10p90
plot_split_normal <- function(fit) {

lower = 0
upper = 100
tol = 0.1

  xs <- seq(lower - 0.1, upper + 0.1, length.out = 2000)
  fx <- split_pdf(xs, mu = fit$mu, sigma_l = fit$sigma_l, sigma_r = fit$sigma_r)
  mass_in_interval <- sum(fx[xs >= lower & xs <= upper]) * (xs[2] - xs[1])
  
  # truncated (theoretical) pdf on a grid
  grid <- seq(lower - tol, upper + tol, length.out = 800)
  theo_pdf <- split_pdf(
    grid,
    mu = fit$mu,
    sigma_l = fit$sigma_l,
    sigma_r = fit$sigma_r
  )
  theo_pdf[!(grid >= lower & grid <= upper)] <- 0
  theo_pdf <- theo_pdf / mass_in_interval
  
 tibble::tibble(x = grid, theo = theo_pdf) |>
   ggplot2::ggplot(ggplot2::aes(x = grid, y = theo_pdf)) +
   ggplot2::geom_line() +
    ggplot2::theme_minimal(base_size = 18) +
     ggplot2::theme(
       axis.text.y = ggplot2::element_blank(),
       axis.title = ggplot2::element_blank(),
       panel.grid.major.y = ggplot2::element_blank(),
       panel.grid.minor.y = ggplot2::element_blank()
     ) +
     ggplot2::labs(
       x = "Proportion of remaining life expectancy spent free of disability (%)"
     )
}
