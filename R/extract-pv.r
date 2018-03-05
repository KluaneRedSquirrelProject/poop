extract_pv <- function(x, first = TRUE) {
  x <- stringr::str_to_lower(x)
  if (first) {
    regex <- "(?<=(pv|poop)[-:;]?\\s{0,2})[a-z]?\\s?[0-9]{3,5}"
    pv <- stringr::str_extract(x, regex)
  } else {
    previous <- "(?<=(pv|poop)[-:;]?\\s{0,2}[a-z]?\\s?[0-9]{3,5}[;\\s]{1,2}(and\\s)?)"
    pv <- stringr::str_extract(x, paste0(previous, "[a-z]{1}[0-9]{3,5}"))
  }
  stringr::str_to_upper(pv)
}
