#' Generate random values from a normal distribution
#'
#' @param n The amount of values to be generated
#' @param mean The mean of the normal distribution to be sampled from
#' @param sd The standard deviation of the normal distribution to be sampled from
#' @param min The smallest acceptable value (defaults to -Inf)
#' @param max The largest acceptable value (defaults to Inf)
#' @param precision Decimals to be rounded to (defaults to 2)
#' @return Some random values
#' @export
generate_numbers <- function(n, mean, sd, min=-Inf, max=Inf,
                             precision=2, try=1) {
  if (try > 100) stop("Giving up after 100 tries. Check parameters")
  result <- rnorm(n, mean, sd) %>%
    round(precision)
  if (min(result) >= min & max(result) <= max) {
    return(result)
  } else {
    return(generate_numbers(n, mean, sd, min, max, precision, try = try + 1))
  }
}
