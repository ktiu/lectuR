#' @export
get_cov <- function(xs=NULL, ys=NULL, meanx=NULL, meany=NULL) {
  result <- list()
  result$formel <- paste("$s_{xy}=\\frac{\\displaystyle \\sum_{i=1}^{n}(x_{i}-",
                         "\\bar{x})\\cdot(y_{i}-\\bar{y})}{n-1}$")
  if ((! is.null(xs)) && (! is.null(ys))) {
    if (is.null(meanx)) meanx <- get_mean(xs)$raw
    if (is.null(meany)) meany <- get_mean(ys)$raw
    zaehler <- sum(round((xs - meanx) * (ys - meany), 2))
    nenner <- length(xs) - 1
    result$einsetzen <- sprintf("$s_{xy}=\\frac{%s}{%s}$", zaehler, nenner)
    result$raw <- round(zaehler / nenner, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$s_{xy}=%s$", result$fmt)
  }
  return(result)
}

#' @export
get_corr <- function(cov=NULL, sdx=NULL, sdy=NULL) {
  result <- list()
  result$formel <- "$r=\\frac{s_{xy}}{s_x \\cdot s_y}$"
  if (length(c(cov, sdx, sdy)) == 3) {
    result$einsetzen <- sprintf("$r=\\frac{%s}{%s \\cdot %s}$",
                                fmt(cov), fmt(sdx), fmt(sdy))
    result$raw <- round(cov / sdx / sdy, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$r=%s$", result$fmt)
    result$interpretieren <- list(
      "eine starke negative",
      "eine mäßig starke negative",
      "eine schwache negative",
      "keine deutliche",
      "eine schwache positive",
      "eine mäßig starke positive",
      "eine starke positive"
    )[[cut(result$raw, breaks = c(-1, -0.8, -0.5, -0.2,
                                  0.2, 0.5, 0.9, 1))]]
  }
  return(result)
}
