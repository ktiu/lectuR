#' @export
tabelle <- function(..., full_width=T, header_above=NULL, sum_row=F,
                    sum_column=F, row_head=F) {
  target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (!is.null(target)) {
    if (target == "html") {
      knitr::kable(..., format = "html") -> wip
    } else if (target == "latex") {
      knitr::kable(...,
                   format = "latex",
                   booktab = T,
                   linesep = "",
                   longtable = F) -> wip
    }
    if (! is.null(header_above)) {
      wip %<>%
        kableExtra::add_header_above(header_above, bold = T)
    }
    if (sum_row) {
      row_count <- nrow(dplyr::first(list(...)))
      wip %<>%
        kableExtra::row_spec(row_count, bold = T) %>%
        kableExtra::row_spec(row_count - 1, hline_after = T,
                             extra_css = "border-bottom: 1px solid; ")
    }
    if (sum_column) {
      col_count <- ncol(dplyr::first(list(...)))
      wip %<>%
        kableExtra::column_spec(col_count, bold = T) %>%
        kableExtra::column_spec(col_count - 1, border_right = T)
    }
    if (row_head) {
      wip %<>%
        kableExtra::column_spec(1, bold = T) %>%
        kableExtra::column_spec(1, border_right = T)
    }
    if (target == "html") {
      wip %>%
        kableExtra::row_spec(0, extra_css = "border-bottom: 1px solid; ") %>%
        kableExtra::kable_styling(full_width = full_width) %>%
        kableExtra::scroll_box(box_css = "overflow-y: auto;")
    } else if (target == "latex") {
      wip %>%
        kableExtra::row_spec(0, bold = T) %>%
        kableExtra::kable_styling()
    }
  } else print("No target format")
}

#' @export
fmt <- function(number, digits=2) {
  number %>%
    format(digits = digits, nsmall = digits, scientific = F) %>%
    stringr::str_replace(",", "{,}") %>%
    return()
}

#' @export
percent <- function() {
  target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if ((!is.null(target)) && target == "latex") {
    return("\\%")
  } else return("%")
}

#' @export
abcparen <- function(number, digits=2) {
  target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if ((!is.null(target)) && target == "latex") {
    return(")")
  } else return("\\)")
}

#' @export
symbol_header <- function(symbol) {
  target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if ((!is.null(target)) && target == "latex") {
    list("z"    = "z",
         "t"    = "t",
         "F"    = "F",
         "sxy"  = "s_{xy}",
         "r"    = "r",
         "R2"   = "R^2",
         "chi2" = "\\chi^2",
         "phi"  = "\\phi")[[symbol]] %>%
      sprintf("$%s$", .)
  } else {
    list("z"    = "𝑧",
         "t"    = "𝑡",
         "F"    = "𝐹",
         "sxy"  = "𝑠<sub>𝑥𝑦</sub>",
         "r"    = "𝑟",
         "R2"   = "𝑅²",
         "chi2" = "𝜒²",
         "phi"  = "𝜙")[[symbol]] %>%
      sprintf("%s", .)
  }
}

fix_formula <- function(string) {
  string %>%
    stringr::str_replace_all("(\\d),(\\d)", "\\1{,}\\2") %>%
    stringr::str_replace_all("- *-", "+") %>%
    stringr::str_replace_all("\\+ *-", "-") %>%
    stringr::str_replace_all("\\cdot *(-[0-9,{}]*)", "\\cdot \\(\\1\\)")
}

#' @export
solution_table <- function(solution_data) {
  summe <- sum(solution_data$Punkte)
  solution_data %>%
    mutate(`Erreicht` = sprintf(paste("<input type='checkbox'",
                                      "style='width:30px;",
                                      "height:30px' value='%s' />"),
                                stringr::str_replace(Punkte, ",", ".")),
           `Max. Punkte` = format(Punkte, nsmall = 1),
           `&nbsp;` = ifelse(Implizit, "(auch&nbsp;implizit)", "")) %>%
    select(Schritt, Musterlösung, `&nbsp;`, `Max. Punkte`, `Erreicht`) %>%
    bind_rows(list(Schritt = "",
                   `Musterlösung` = "",
                   `&nbsp;` = "$\\sum$",
                   `Max. Punkte` = format(summe, nsmall = 1),
                   `Erreicht` =  paste0("<div class='punkte-aufgabe'>",
                                      "<input type='hidden' value='1'/>",
                                      "<span></span>"))) %>%
    tabelle(escape = F, align = "lccc")
}
