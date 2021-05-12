#' @export
get_z1 <- function(data  = NULL,
                   barx  = NULL,
                   mu    = NULL,
                   sigma = NULL,
                   alpha = 0.05,
                   mode  = c("ungerichtet", "aufwärts", "abwärts")) {
  result <- list()
  result$formel <- "$z=\\sqrt{n}\\cdot\\frac{\\bar{x}-\\mu_0}{\\sigma}$"
  result$wählen <- paste(
    "Der Mittelwert einer Stichprobe soll auf signifikante Abweichung von der",
    "Grundgesamtheit geprüft werden. Die Standardabweichung der Population ist",
    "bekannt. Deshalb wird ein $z$-Test durchgeführt.")
  if (!is.null(data)) {
    if (is.null(barx)) barx <- get_mean(data)$raw
  }
  if (length(c(barx, mu, sigma, alpha, mode)) == 5) {
    result$nullhypothese       <- "$H_0: \\mu = \\mu_0$"
    result$alternativhypothese <- "$H_0: \\mu %s \\mu_0$" %>%
      sprintf(c("ungerichtet" = "\\neq",
                "aufwärts"    = "\\gt",
                "abwärts"     = "\\lt")[mode])
    result$signifikanz <- "$\\alpha = %s$" %>%
      sprintf(alpha) %>%
      fix_formula
    result$ablehnungsbereich <- list(
      abwärts = list(
        formel     = "$z \\leq z_{\\alpha}$",
        einsetzen  = "$z \\leq z_{%s\\%%}$" %>%
          sprintf(alpha * 100) %>%
          fix_formula,
        lower      = floor(qnorm(alpha) * 100) / 100,
        upper      = Inf,
        ergebnis   = sprintf("$z \\leq %s$",
                             fmt(floor(qnorm(alpha) * 100) / 100))
      ),
      aufwärts = list(
        formel     = "$z \\geq z_{(1-\\alpha)}$",
        einsetzen  = "$z \\geq z_{%s\\%%}$" %>%
          sprintf(100 - alpha * 100) %>%
          fix_formula,
        lower      = -Inf,
        upper      = ceiling(qnorm(1 - alpha) * 100) / 100,
        ergebnis   = sprintf("$z \\geq %s$",
                             fmt(ceiling(qnorm(1 - alpha) * 100) / 100))
      ),
      ungerichtet = list(
        formel     = paste("$z \\leq z_{\\alpha/2}\\quad \\textrm{oder}",
                           "\\quad z \\geq z_{(1-\\alpha/2)}$"),
        einsetzen  = paste("$z \\leq z_{%s\\%%} \\quad \\textrm{oder}",
                           "\\quad z \\geq z_{%s\\%%}$") %>%
          sprintf(alpha / 2 * 100, 100 - alpha / 2 * 100) %>%
          fix_formula,
        lower      = floor(qnorm(alpha / 2) * 100) / 100,
        upper      = ceiling(qnorm(1 - alpha / 2) * 100) / 100,
        ergebnis   = "$z \\leq %s\\quad \\textrm{oder} \\quad z \\geq %s$" %>%
          sprintf(fmt(floor(qnorm(alpha / 2) * 100) / 100),
                  fmt(ceiling(qnorm(1 - alpha / 2) * 100) / 100))
      )
    )[[mode]]
    result$einsetzen <- "$z=\\sqrt{%s}\\cdot\\frac{%s-%s}{%s}$" %>%
      sprintf(length(data), barx, mu, sigma) %>%
      fix_formula
    result$raw      <- round(sqrt(length(data)) * (barx - mu) / sigma, 2)
    result$fmt      <- fmt(result$raw)
    result$ergebnis <- sprintf("$z\\approx%s$", result$fmt)
    result$test     <- (result$raw <= result$ablehnungsbereich$lower |
                        result$raw >= result$ablehnungsbereich$upper)
    result$interpretieren$ablehnungsbereich <-
      "Der Ablehnungsbereich wurde %serreicht." %>%
      sprintf(ifelse(result$test, "", "nicht "))
    result$interpretieren$hypothese <- "Die Nullhypothese wird %s." %>%
      sprintf(ifelse(result$test, "abgelehnt", "beibehalten"))
  }
  return(result)
}
