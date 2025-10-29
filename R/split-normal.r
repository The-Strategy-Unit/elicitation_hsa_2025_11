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

#' Plot split normal
#' Generates and then plots a split normal from
#' the mode, and two sd (left and right)
#' @param fit object returned from est_spnorm_from_p10p90
plot_split_normal <- function(fit) {
  fanplot::rsplitnorm(
    1e6,
    mode = fit$mu,
    sd1 = fit$sigma_l,
    sd2 = fit$sigma_r
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(is.finite(.data$value)) |>
    dplyr::filter(.data$value > 0, .data$value < 100) |>
    ggplot2::ggplot() +
    ggplot2::geom_density(ggplot2::aes(x = .data$value / 100)) + # divide by 100 to use scale percent
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Proportion of remaining life expectancy spent free of disability (%)"
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, 1))
}
