#' @export
get_b <- function(cov=NULL, varx=NULL, sdx=NULL) {
  result <- list()
  result$formel <- "$b=\\frac{s_{xy}}{s^2_x}$"
  if(!is.null(cov) & length(c(cov, sdx, varx) > 1)) {
    nennerfmt <- ifelse(is.null(varx),
                        sprintf("%s^2", fmt(sdx)),
                        str_replace(varx, ",", "{,}"))
    nenner <- ifelse(is.null(varx), round(sdx^2, 2), varx)
    result$einsetzen <- sprintf("$b=\\frac{%s}{%s}$",
                                str_replace(cov, ",", "{,}"),
                                nennerfmt)
    result$raw <- round(cov/nenner,3)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$b=%s$", result$fmt)
  }
  return(result)
}

#' @export
get_a <- function(b=NULL, meanx=NULL, meany=NULL) {
  result <- list()
  result$formel <- "$a=\\bar{y}-b\\cdot\\bar{x}$"
  if(length(c(b, meanx, meany)==3)){
    result$einsetzen <- sprintf("$a=%s-%s\\cdot%s$",
                                str_replace(meany, ",", "{,}"),
                                str_replace(b, ",", "{,}"),
                                str_replace(meanx, ",", "{,}")) %>%
                          fix_formula
    result$raw <- round(meany - b*meanx, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$a=%s$", result$fmt)
  }
  return(result)
}

#' @export
get_reg <- function(a=NULL, b=NULL){
  result <- list()
  result$formel <- "$y=a + b \\cdot x$"
  if(length(c(a, b) == 2)){
    result$ergebnis <- sprintf("$y \\approx %s + %s \\cdot x$",
                               str_replace(a, ",", "{,}"),
                               str_replace(b, ",", "{,}")
                              ) %>% fix_formula
  }
  return(result)
}

#' @export
get_pred <- function(a, b, x=NULL, y=NULL, symbol=NULL) {
  haty <- ifelse(! is.null(symbol),
                 sprintf("\\hat{y}_{%s}", symbol),
                 "\\hat{y}")

  x_ <- ifelse(! is.null(symbol),
                 sprintf("x_{%s}", symbol),
                 "x")

  result <- list()
  result$formel <- sprintf("$%s= %s + %s \\cdot %s$", haty, a, b, x_) %>%
    fix_formula
  if(length(c(a, b, x)) == 3){
    result$einsetzen <- "$%s\\approx %s + %s \\cdot %s$" %>%
      sprintf(haty, a, b, x) %>%
      fix_formula
    result$raw <- round(a + b * x, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$%s\\approx%s$", haty, result$fmt)
  } else if (length(c(a, b, y)) == 3) {
    result$formel <- sprintf("$%s = \\frac{%s - %s}{%s}$", x_, haty, a, b) %>%
      fix_formula
    result$einsetzen <- "$%s \\approx \\frac{%s-%s}{%s}$" %>%
      sprintf(x_, y, a, b) %>%
      fix_formula
    result$raw <- round((y - a) / b, 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$%s\\approx%s$", x_, result$fmt)
  }
  return(result)
}
