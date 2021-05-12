#' @export
get_phi <- function(chisq=NULL, n=NULL) {
  result <- list()
  result$formel <- "$\\phi=\\sqrt{\\frac{\\chi^2}{n}}$"
  if(length(c(chisq, n)) == 2) {
    result$einsetzen <- "$\\phi\\approx\\sqrt{\\frac{%s}{%s}}$" %>%
      sprintf(chisq, n) %>%
      fix_formula
    result$raw <- round(sqrt(chisq/n), 2)
    result$fmt <- fmt(result$raw)
    result$ergebnis <- sprintf("$\\phi\\approx%s$", result$fmt)
  }
  return(result)
}

#' @export
get_ci <- function(chisq=NULL, n=NULL, k=NULL, l=NULL) {
  result <- list()
  result$formel <- paste0("$\\mathit{CI}=\\sqrt{\\frac{\\chi^2}",
                          "{n \\cdot (\\mathrm{min}(k, \\ell)-1)}}$")

  if (length(c(chisq, n, k, l)) == 4) {
    result$einsetzen <- paste0("$\\mathit{CI}\\approx\\sqrt{\\frac{%s}",
                            "{%s \\cdot (%s - 1)}}$") %>%
      sprintf(chisq, n, min(k, l)) %>%
      fix_formula
    result$raw <- round(sqrt(chisq / (n * (min(k, l) - 1))), 3)
    result$fmt <- fmt(result$raw, 3)
    result$ergebnis <- sprintf("$\\mathit{CI}\\approx%s$", result$fmt)
  }
  return(result)
}

#' @export
get_chisq <- function(xt=NULL) {
  result <- list()
  result$formel <- paste0("$\\chi^2= \\sum_{i=1}^{k}\\sum_{j=1}^{\\ell}",
                          "\\frac{(n_{ij}-m_{ij})^{2}}{m_{ij}}$")
  if (! is.null(xt)) {
    xt$table %>%
      select(starts_with("x_")) %>%
      unlist() -> partial
    result$einsetzen <- sprintf("$\\chi^2\\approx %s$",
                                partial %>%
                                  fmt() %>%
                                  paste(collapse = "+")) %>%
      fix_formula
    result$raw <- round(sum(partial), 3)
    result$fmt <- fmt(result$raw, 3)
    result$ergebnis <- sprintf("$\\chi^2\\approx %s$", result$fmt)
  }
  return(result)
}

#' @export
construct <- function(counts, ...) {
  variables <- rev(list(...))
  stopifnot(length(variables) == 2,
            length(counts) == length(variables[[1]]) * length(variables[[2]]))
  do.call(tidyr::expand_grid, variables) %>%
    add_column(counts) %>%
    purrr::pmap(function(...) {
      l <- list(...)
      tibble(var1 = rep(l[[1]], l[[3]]),
             var2 = rep(l[[2]], l[[3]]))
    }) %>%
    reduce(rbind) %>%
    .[sample(nrow(.)), ] %>%
    rename_with(function(x) {
                  names(variables)[readr::parse_number(x)]
    })
}

#' Prepare crosstable from a dataset
#' @param data Long-form dataset (tibble) with one or two nominal type variables
#' @param row_order Optional vector to specify the order of the first variable
#' @param col_order Optional vector to specify the order of the second variable
#' @return A list containing a specially formatted crosstable and additional information for use in `print_crosstable()`
#' @export
get_crosstable <- function(data, row_order=NULL, col_order=NULL) {
  xt <- table(data)
  if (is.na(ncol(xt))) {
    if (is.null(row_order)) row_order <- rownames(xt)
    col_order <- NULL
    tibble(n = xt,
           m = round(sum(xt) / nrow(xt), 2)) %>%
      mutate(x = round((n - m) ^ 2 / m, 3)) ->
      table
  } else {
    if (is.null(row_order)) row_order <- rownames(xt)
    if (is.null(col_order)) col_order <- colnames(xt)
    purrr::map2(col_order, seq_along(col_order), function(col, j) {
      n  <- purrr::map_int(row_order, ~ xt[., col])
      ni <- purrr::map_int(row_order, ~ sum(xt[., ]))
      nj <- sum(xt[, col])
      m  <- round(ni * nj / nrow(data), 2)
      x  <- round((n - m) ^ 2 / m, 3)
      tibble("n_{j}" := n,
             "m_{j}" := m,
             "x_{j}" := x)
    }) %>%
    reduce(add_column) -> table
  }
  list(table = table,
       vars  = names(data),
       rows  = row_order,
       cols  = col_order)
}

#' @export
print_crosstable <- function(xt, variable_names=T, question=F, sums=T,
                             expected=T, chisq=T, fragment=NA, caption=NULL) {
  if (question) {
    sums     <- F
    expected <- F
    chisq    <- F
  }
  target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(target)) {
    stop("No output target specified. Are you knitting?")
  } else if (target == "latex") {
    template <- c("${n[i]}",
                  c("(${m[i]})",
                    "\\textcolor{goethe_blue}{${x[i]}}")[c(expected,
                                                           chisq)]) %>%
      paste(collapse = "\\\\") %>%
      paste0("\\makecell[tr]{", ., "}")
  } else if (target == "html") {
    template <- c("${n[i]}",
                  c("(${m[i]})",
                    "<span class='blue-text'>${x[i]}</span>")[c(expected,
                                                                chisq)]) %>%
      paste(collapse = "<br />")
  } else stop("Unknown output target.")
  if (is.null(xt$cols)) { # one dimension
    header_above <- NULL
    last_column <- NULL
    if (sums) {
      last_column <- tibble("  " = sum(xt$table$n)) %>%
        list()
    }
    seq_along(xt$rows) %>%
      purrr::map(function(i) {
        n <- xt$table$n
        m <- xt$table$m
        x <- xt$table$x
        tibble::tibble("{xt$rows[i]}" := stringr::str_interp(template))
      }) %>%
      c(last_column) %>%
      reduce(cbind) -> tbl
    sum_row <- F
    row_head <- F
  } else { # two dimensions
    if (variable_names) {
      first_col_name <- sprintf("%s ↓", xt$vars[1])
      header_above <- c(" ", cols = length(xt$cols))
      names(header_above) <- c("", sprintf("→ %s", xt$vars[2]))
    } else {
      first_col_name <- " "
      header_above <- NULL
    }
    sum_i <- xt$table %>%
      select(starts_with("n_")) %>%
      rowSums()
    n_all <- sum(sum_i)
    if (sums) {
      first_column <- tibble("{first_col_name}" := c(xt$rows, "")) %>% list()
      last_column <- tibble("  " = c(sum_i, n_all)) %>% list()
      if (! is.null(header_above)) header_above <- c(header_above, " ")
    } else {
      first_column <- tibble("{first_col_name}" := xt$rows) %>% list()
      last_column <- NULL
    }
    seq_along(xt$cols) %>%
      purrr::map(function(j) {
        n <- xt$table[[sprintf("n_%s", j)]]
        m <- xt$table[[sprintf("m_%s", j)]]
        x <- xt$table[[sprintf("x_%s", j)]]
        seq_along(xt$rows) %>%
          purrr::map(function(i) {
            stringr::str_interp(template)
          }) -> cells
        if (sums) cells <- c(cells, sum(n))
        tibble::tibble("{xt$cols[j]}" := cells)
      }) %>%
      c(first_column, ., last_column) %>%
      reduce(cbind) -> tbl
      sum_row <- sums
      row_head <- T
    }
    tabelle(tbl, escape = F, align = "r", row_head = row_head,
            header_above = header_above, caption = caption, sum_row = sum_row,
            sum_column = sums)
}

