#' @export
tabelle <- function(..., full_width=T) {
  target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (!is.null(target)) {
    if (target == "html") {
      knitr::kable(..., format="html") %>%
        kableExtra::kable_styling(full_width = full_width) %>%
        kableExtra::scroll_box(box_css = "overflow-y: auto;")
    } else if (target == "revealjs") {
    } else if (target == "latex") {
      knitr::kable(...,
            format = "latex",
            booktab = T,
            linesep = "",
            longtable = F) %>%
        kableExtra::kable_styling()
    }
  }
}

#' @export
fmt <- function(number, digits=2) {
  number %>%
    format(digits = digits, nsmall = digits, scientific = F) %>%
    str_replace(",", "{,}") %>%
    return()
}

#' @export
percent <- function() {
  if ((!is.null(target)) && target == "latex") {
    return("\\%")
  } else return("%")
}

#' @export
abcparen <- function(number, digits=2) {
  if ((!is.null(target)) && target == "latex") {
    return(")")
  } else return("\\)")
}

