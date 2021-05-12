#' @export
get_f <- function(datalist = NULL,
                  mode = c("ungerichtet", "aufwärts", "abwärts"),
                  var1 = NULL,
                  var2 = NULL,
                  n1 = NULL,
                  n2 = NULL,
                  alpha = 0.05) {
  result <- list()
  result$formel <- "$F = \\frac{s^2_1}{s^2_2}$"
  result$wählen <- paste("Zwei Stichproben sollen auf einen signifkanten",
                         "Unterschied in der Varianz überprüft werden. Deshalb",
                         "muss ein $F$-Test durchgeführt werden.")
  if (!is.null(datalist)) {
    if (is.null(var1)) var1 <- get_var(datalist[[1]])$raw
    if (is.null(var2)) var2 <- get_var(datalist[[2]])$raw
    if (is.null(n1))   n1   <- length(datalist[[1]])
    if (is.null(n2))   n2   <- length(datalist[[2]])
  }
  if (length(c(var1, var2, n1, n2, mode, alpha)) == 6) {
    df1 <- n1-1
    df2 <- n2-1
    result$nullhypothese <- "$H_0: \\sigma^2_1 = \\sigma^2_2$"
    result$alternativhypothese <- "$H_1: \\sigma^2_1 %s \\sigma^2_2$" %>%
      sprintf(c("ungerichtet" = "\\neq",
                "aufwärts"    = "\\gt",
                "abwärts"     = "\\lt")[mode])
    result$signifikanz <- "$\\alpha = %s$" %>%
      sprintf(alpha) %>%
      fix_formula
    result$df1 <- "$\\mathit{df}_1 = %s-1 = %s$" %>%
      sprintf(n1, df1)
    result$df2 <- "$\\mathit{df}_2 = %s-1 = %s$" %>%
      sprintf(n2, df2)
    result$ablehnungsbereich <- list(
      abwärts = list(
        formel     = "$F \\leq F_{\\mathit{df}_1;\\mathit{df}_2;\\alpha}$",
        einsetzen  = "$F \\leq F_{%s;%s;%s\\%%}$" %>%
          sprintf(df1, df2, alpha * 100) %>%
          fix_formula,
        lower      = round(qf(alpha, df1, df2), 2),
        upper      = Inf,
        ergebnis   = sprintf("$F \\leq %s$", fmt(qf(alpha, df1, df2)))
      ),
      aufwärts = list(
        formel     = "$F \\geq F_{\\mathit{df}_1;\\mathit{df}_2;(1-\\alpha)}$",
        einsetzen  = "$F \\geq F_{\\mathit{%s};\\mathit{%s};%s\\%%}$" %>%
          sprintf(df1, df2, 100 - alpha * 100) %>%
          fix_formula,
        lower      = -Inf,
        upper      = round(qf(1 - alpha, df1, df2), 2),
        ergebnis   = sprintf("$F \\geq %s$", fmt(qf(1 - alpha, df1, df2)))
      ),
      ungerichtet = list(
        formel     = "$F \\leq F_{\\mathit{df_1};\\mathit{df_2};\\alpha/2}\\quad \\textrm{oder} \\quad F \\geq F_{\\mathit{df_1};\\mathit{df_2};(1-\\alpha/2)}$",
        einsetzen  = "$F \\leq F_{%s;%s;{%s\\%%}} \\quad \\textrm{oder} \\quad F \\geq F_{%s;%s;{%s\\%%}}$" %>%
          sprintf(df1, df2, alpha/2*100, df1, df2, 100-alpha/2*100) %>%
          fix_formula,
        lower      = round(qf(alpha/2, df1, df2), 2),
        upper      = round(qf(1-alpha/2, df1, df2), 2),
        ergebnis   = "$F \\leq %s\\quad \\textrm{oder} \\quad F \\geq %s$" %>%
          sprintf(fmt(qf(alpha/2, df1, df2), 2),
                  fmt(qf(1-alpha/2, df1, df2), 2))
      )
    )[[mode]]
    result$einsetzen <- "$F = \\frac{%s}{%s}$" %>%
      sprintf(var1,  var2) %>% fix_formula
    result$raw       <- round(var1/var2, 2)
    result$fmt       <- fmt(result$raw)
    result$ergebnis  <- "$F = %s$" %>% sprintf(result$fmt)
    result$test      <- (result$raw <= result$ablehnungsbereich$lower | result$raw >= result$ablehnungsbereich$upper)
    result$interpretieren$ablehnungsbereich <- "Der Ablehnungsbereich wurde %serreicht." %>%
      sprintf(ifelse(result$test, "", "nicht "))
    result$interpretieren$hypothese <- "Die Nullhypothese wird %s." %>%
      sprintf(ifelse(result$test, "abgelehnt", "beibehalten"))
  }
  return(result)
}
