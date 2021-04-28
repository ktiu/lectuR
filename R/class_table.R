#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import kableExtra

#' @export
klassieren <- function(data, unit, breaks = NULL){

  if (is.null(breaks)) breaks <- pretty(data)

  breaks %>%
    as.list() %>%
    reduce(function(acc, x) {
     if (! is.null(acc$prev)) {
       this_class <- list(
         label = sprintf("von %s bis unter %s %s", acc$prev, x, unit),
         mean  = mean(c(acc$prev, x))
       )
       acc$classes[[as.character(x)]] <- this_class
     }
     acc$prev <- x
     return(acc)
  }, .init = list(prev = NULL, classes = list())) %>%
    .$classes -> classes

  cut(data,
      right = F,
      breaks = breaks) %>%
    table() -> freq

  tibble(x = map_chr(classes, "label"),
         k = map_dbl(classes, "mean"),
         f = freq,
         fkum = cumsum(freq)) %>%
    mutate(prod = k * f) -> tbbl

  round(sum(tbbl$prod) / length(data), 2) -> barx

  tbbl %>%
    mutate(diff = k - barx,
           diffsq = round(diff^2, 2),
           prodsq = f * diffsq)
}
