#' @export
get_mean <- function(data = NULL, symbol = "x") {
  result <- list()
  result$formel <- sprintf("$\\bar{%s}=\\frac{\\sum\\limits_{i=1}^{n}%s_{i}}{n}$",
                           symbol,
                           symbol)
  if(!is.null(data)){
    result$einsetzen <- sprintf("$\\bar{%s}=\\frac{%s}{%s}$",
                                symbol, fmt(sum(data)),
                                length(data))
    result$raw       <- round(mean(data), 2)
    result$fmt       <- fmt(result$raw)
    result$ergebnis  <- sprintf("$\\bar{%s}=%s$", symbol, result$fmt)
  }
  return(result)
}

#' @export
get_median <- function(data, alt = F, symbol = "x") {
  label  <- ifelse(alt, sprintf("\\textit{Md}_%s", symbol), "\\textit{Md}")
  sorted <- sort(data)
  result <- list()
  if(length(data) %% 2 == 0) {
    result$formel <- "$%s = \\frac{x_{(\\frac{n}{2})}+x_{(\\frac{n}{2}+1)}}{2}$" %>%
      sprintf(label)
    result$einsetzen <- "$%s=\\frac{x_{%s}+x_{%s}}{2}=\\frac{%s + %s}{2}$" %>%
      sprintf(label, length(sorted)/2, length(sorted)/2+1, sorted[length(sorted)/2], sorted[length(sorted)/2+1]) %>%
      fix_formula
    result$raw      <- round(median(data), 2)
    result$fmt      <- fmt(result$raw)
    result$ergebnis <- sprintf("$%s=%s$", label, result$fmt)
  } else {
    result$formel <- "$%s=x_{(\\frac{n+1}{2})}$" %>%
      sprintf(label)
    result$einsetzen <- "$%s=x_{(%s)}$" %>%
      sprintf(label, (length(sorted)+1)/2)
    result$raw      <- round(median(data), 2)
    result$fmt      <- fmt(result$raw)
    result$ergebnis <- sprintf("$%s=%s$", label, result$fmt)
  }
  return(result)
}

#' @export
get_var <- function(data = NULL, alt = F, symbol = "x") {
  label <- ifelse(alt, sprintf("s^2_%s", symbol), "s^2")
  result <- list()
  result$formel <- sprintf("$%s=\\frac{\\sum\\limits_{i=1}^{n}(%s_{i}-\\bar{%s})^2}{n-1}$",
                           label,
                           symbol,
                           symbol)
  if(!is.null(data)) {
    result$einsetzen <- sprintf("$%s=\\frac{%s}{%s}$",
                                label,
                                fmt(sum((data-round(mean(data), 2))^2)),
                                length(data)-1)
    result$raw       <- round(sum(round((data-round(mean(data), 2))^2,2))/(length(data)-1), 2)
    result$fmt       <- fmt(result$raw)
    result$ergebnis  <- sprintf("$%s=%s$", label, result$fmt)
  }
  return(result)
}

#' @export
get_sd <- function(data = NULL, alt = F, symbol = "x", variance = NULL, pop = F) {
  base_label <- ifelse(pop, "\\sigma", "s")
  label      <- ifelse(alt, sprintf("%s_%s", base_label, symbol), base_label)
  labels2    <- ifelse(alt, sprintf("%s^2_%s", base_label, symbol), sprintf("%s^2", base_label))
  result     <- list()
  result$formel <- sprintf("$%s=\\sqrt{%s}$", label, labels2)
  if(!is.null(data)) {
    variance         <- get_var(data)$raw
  }
  if(!is.null(variance)) {
    result$einsetzen <- sprintf("$%s=\\sqrt{%s}$", label, fmt(variance))
    result$raw       <- round(sqrt(variance), 2)
    result$fmt       <- fmt(result$raw)
    result$ergebnis  <- sprintf("$%s\\approx%s$", label, result$fmt)
  }
  return(result)
}

#' @export
get_range <- function(data=NULL, alt=F, symbol="x") {
  label  <- ifelse(alt, sprintf("R_%s", symbol), "R")
  result <- list()
  result$formel <- sprintf("$%s=x_{(n)}-x_{(1)}$", label)
  if(!is.null(data)) {
    sorted <- sort(data)
    first  <- sorted[1]
    last   <- sorted[length(data)]
    result$einsetzen <- sprintf("$%s=%s-%s$", label, last, first) %>% fix_formula
    result$raw <- last-first
    result$fmt <- str_replace(result$raw, ",", "{,}")
    result$ergebnis <- sprintf("$%s=%s$", label, result$fmt)
  }
  return(result)
}

#' @export
get_iqr <- function(data=NULL, alt=F, symbol="x") {
  label  <- ifelse(alt, sprintf("\\mathit{IQR}_%s", symbol), "\\mathit{IQR}")
  result <- list()
  result$formel <- sprintf("$%s=Q_3-Q_1$", label)
  if(!is.null(data)) {
    sorted <- sort(data)
    q1 <- median(sorted[1:ceiling(length(data)/2)])
    q3 <- median(sorted[floor(length(data)/2+1):length(data)])
    result$q1 <- "$%s=%s$" %>% sprintf("Q_1", q1) %>% fix_formula()
    result$q3 <- "$%s=%s$" %>% sprintf("Q_3", q3) %>% fix_formula()
    result$einsetzen <- sprintf("$%s=%s-%s$", label, q3, q1) %>% fix_formula()
    result$raw <- q3 - q1
    result$fmt <- stringr::str_replace(result$raw, ",", "{,}")
    result$ergebnis <- sprintf("$%s=%s$", label, result$fmt)
  }
  return(result)
}

#' @export
get_variationskoeffizient <- function(data = NULL,
                                      mean=NULL,
                                      sd=NULL,
                                      alt=F,
                                      symbol="x") {
   label <- ifelse(alt, sprintf("v_%s", symbol), "v")
   result <- list(
    formel = "$%s=\\frac{s}{|\\bar{x}|}\\cdot100\\%%\\quad$" %>%
      sprintf(label)
   )
   if (!is.null(data)) {
     if (is.null(mean)) mean <- get_mean(data)$raw
     if (is.null(sd))   sd   <- get_sd(data)$raw
   }
   if (length(c(mean, sd)) == 2) {
     result$einsetzen <- "$%s=\\frac{%s}{%s}\\cdot100\\%%$" %>%
      sprintf(label, sd, abs(mean)) %>%
      fix_formula
     result$raw <- round(sd / mean * 100, 2)
     result$fmt <- fmt(result$raw)
     result$ergebnis <- "$%s=%s\\%%$" %>%
      sprintf(label, result$fmt)
   }
   return(result)
}
