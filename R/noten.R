#' @export
runden <- function(note) {
  if(!(note <= 6 && note >= 0)) stop("note must be between 0 and 6")
  pre <- round(round((note * 3) - 0.1) / 3, 1)
  if (pre > 4) pre <- 5
  if (pre < 1) pre <- 1
  return(pre)
}

#' @export
punkte <- function(note) {
  if(!(note <= 6 && note >= 0)) stop("note must be between 0 and 6")
  pre <- round(17 - note * 3)
  if (pre < 5) pre <- 0
  if (pre > 15) pre <- 15
  return(pre)
}

check_weighting <- function(kriterien) {
  w <- sum(purrr::map_dbl(kriterien, "gewichtung"))
  if (! w == 1) stop(
    sprintf("Gewichtungen müssen 1 ergeben (jetzt: %s)", w)
  )
}

check_integrity <- function(noten, kriterien) {
  abstract <- sort(purrr::map_chr(kriterien, "kurz"))
  concrete <- sort(unique(
    purrr::map_chr(purrr::flatten(
      purrr::map(noten, "teilnoten")
    ), "aspekt")
  ))
  if (! all(concrete == abstract)) stop(
    sprintf("Kriterien nicht kongruent. Diff: %s",
            paste(c(setdiff(concrete, abstract),
                   setdiff(abstract, concrete)),
                  collapse = "; ")
    )
  )
}

check_complete_single <- function(note, kriterien, arg_check) {
  if (! all(typeof(note$projekt) == "character",
            note$projekt != "")) {
    ArgumentCheck::addError(
      sprintf("Projekt ohne Slug"),
      arg_check
    )
  }
  if (! all(typeof(note$titel) == "character",
            note$titel != "")) {
    ArgumentCheck::addError(
      sprintf("%s: Titel nicht vorhanden", note$projekt),
      arg_check
    )
  }

  purrr::walk(note$teilnoten, function(tn) {
    if (! all(typeof(tn$kommentar) == "character",
              tn$kommentar != "")) {
      ArgumentCheck::addError(
        sprintf("%s: Kommentar für %s nicht vorhanden",
                note$projekt,
                tn$aspekt),
        arg_check
      )
    }
    if (! all(typeof(tn$note) == "integer",
              tn$note > 0,
              tn$note < 6)) {
      ArgumentCheck::addError(
        sprintf("%s: Ungültige Note für %s: %s",
                note$projekt,
                tn$aspekt,
                tn$note),
        arg_check
      )
    }
  })
}

check_complete <- function(noten, kriterien) {
  arg_check <- ArgumentCheck::newArgCheck()
  purrr::walk(noten, function(n) {
    check_complete_single(n, kriterien, arg_check)
  })
  ArgumentCheck::finishArgCheck(arg_check)
}

kriterien_mean <- function(noten, kriterien) {
  purrr::map(kriterien, function(krit) {
    purrr::map_int(noten, function(n) {
      purrr::keep(n$teilnoten, function(x) {
        x$aspekt == krit$kurz
      })[[1]]$note
    }) -> noten
    krit$mean <- mean(noten)
    return(krit)
  })
}

#' @export
einlesen <- function(noten_file="noten.yml",
                     kriterien_file="kriterien.yml") {
  noten <- yaml::read_yaml(noten_file)
  kriterien <- yaml::read_yaml(kriterien_file)
  check_weighting(kriterien)
  check_integrity(noten, kriterien)
  check_complete(noten, kriterien)
  km_mean <- kriterien_mean(noten, kriterien)

  purrr::map(noten, function(projekt) {

    # Mit Kriterien verschneiden
    purrr::map(projekt$teilnoten, function(tn) {
      purrr::keep(km_mean, function(x) {
        x$kurz == tn$aspekt
      })[[1]] -> krit
      purrr::flatten(list(tn, krit))
    }) -> projekt$teilnoten

    # Noten errechnen
    purrr::reduce(projekt$teilnoten, function(acc, tn) {
      acc + tn$gewichtung * tn$note
    }, .init = 0) -> pre
    projekt$note <- runden(pre)
    projekt$notenpunkte <- punkte(projekt$note)
    return(projekt)
  })
}
