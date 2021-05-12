#' @export
get_t1 <- function(data  = NULL,
                   barx  = NULL,
                   s     = NULL,
                   n     = NULL,
                   mu    = NULL,
                   alpha = 0.05,
                   mode  = c("ungerichtet", "aufwärts", "abwärts")) {
  result <- list()
  result$formel <- "$t=\\sqrt{n}\\cdot\\frac{\\bar{x}-\\mu_0}{s}$"
  result$wählen <- paste("Der Mittelwert einer Stichprobe soll auf",
                         "signifikante Abweichung von der Grundgesamtheit",
                         "geprüft werden. Die Standardabweichung der",
                         "Population ist nicht bekannt. Deshalb wird ein",
                         "$t$-Test durchgeführt.")
  if (!is.null(data)) {
    if (is.null(barx)) barx <- get_mean(data)$raw
    if (is.null(s))    s    <- get_sd(data)$raw
    if (is.null(n))    n    <- length(data)
  }
  if (length(c(barx, mu, s, n, alpha, mode)) == 6) {
    df <- n - 1
    result$nullhypothese       <- "$H_0: \\mu = \\mu_0$"
    result$alternativhypothese <- "$H_0: \\mu %s \\mu_0$" %>%
      sprintf(c("ungerichtet" = "\\neq",
                "aufwärts"    = "\\gt",
                "abwärts"     = "\\lt")[mode])
    result$signifikanz <- "$\\alpha = %s$" %>%
      sprintf(alpha) %>%
      fix_formula
    result$df <- "$\\mathit{df} = n -1 = %s - 1 = %s$" %>%
      sprintf(n, df)
    result$ablehnungsbereich <- list(
      abwärts = list(
        formel     = "$t \\leq t_{\\mathit{df};\\alpha}$",
        einsetzen  = "$t \\leq t_{%s;%s\\%%}$" %>%
          sprintf(df, alpha * 100) %>%
          fix_formula,
        lower      = round(qt(alpha, df), 3),
        upper      = Inf,
        ergebnis   = sprintf("$z \\leq %s$", round(qt(alpha, df), 3)) %>%
          fix_formula
      ),
      aufwärts = list(
        formel     = "$t \\geq t_{\\mathit{df}; (1-\\alpha)}$",
        einsetzen  = "$t \\geq t_{%s; %s\\%%}$" %>%
          sprintf(df, 100 - alpha * 100) %>%
          fix_formula,
        lower      = -Inf,
        upper      = round(qt(1 - alpha, df) ,3),
        ergebnis   = sprintf("$t \\geq %s$", round(qt(1-alpha, df),3)) %>%
          fix_formula
      ),
      ungerichtet = list(
        formel     = "$t \\leq t_{\\mathit{df};\\alpha/2}\\quad \\textrm{oder} \\quad t \\geq t_{\\mathit{df};(1-\\alpha/2)}$",
        einsetzen  = "$t \\leq t_{%s; %s\\%%} \\quad \\textrm{oder} \\quad t \\geq t_{%s; %s\\%%}$" %>%
          sprintf(df, alpha/2*100, df, 100-alpha/2*100) %>%
          fix_formula,
        lower      = round(qt(alpha/2, df),3),
        upper      = round(qt(1-alpha/2, df),3),
        ergebnis   = "$t \\leq %s\\quad \\textrm{oder} \\quad t \\geq %s$" %>%
          sprintf(round(qt(alpha/2, df),3),
                  round(qt(1-alpha/2, df),3)) %>%
        fix_formula
      )
    )[[mode]]
    result$einsetzen <- "$t=\\sqrt{%s}\\cdot\\frac{%s-%s}{%s}$" %>%
      sprintf(n, barx, mu, s) %>%
      fix_formula
    result$raw      <- round(sqrt(n)*(barx-mu)/s, 2)
    result$fmt      <- fmt(result$raw)
    result$ergebnis <- sprintf("$t\\approx%s$", result$fmt)
    result$test     <- (result$raw <= result$ablehnungsbereich$lower | result$raw >= result$ablehnungsbereich$upper)
    result$interpretieren$ablehnungsbereich <- "Der Ablehnungsbereich wurde %serreicht." %>%
      sprintf(ifelse(result$test, "", "nicht "))
    result$interpretieren$hypothese <- "Die Nullhypothese wird %s." %>%
      sprintf(ifelse(result$test, "abgelehnt", "beibehalten"))
  }
  return(result)
}

