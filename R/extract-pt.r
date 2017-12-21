extract_pt <- function(x, first = TRUE) {
  x <- stringr::str_to_lower(x)
  # first look for a PT
  rgex <- "(?<=p(t|r)[-;,:]?\\s{0,2})[0-2]?[0-9](h|:)?[0-9]{2}\\s?(am|pm)?"
  pt <- stringr::str_extract(x, rgex)
  # in case PT is missing
  pv <- "(?<=pv[-:;]?\\s{0,2}[a-z]?\\s?[0-9]{3,5}[;,:\\sa-z]{1,6})"
  rgex <- "[0-2]?[0-9](h|:)?[0-9]{2}\\s?(am|pm)?"
  no_pt <- stringr::str_extract(x, paste0(pv, rgex))
  pt <- dplyr::coalesce(pt, no_pt)
  clean_pt(pt)
}

clean_pt <- function(x) {
  x <- stringr::str_to_lower(x)
  # standardize format
  is_pm <- !is.na(x) & str_detect(x, "pm$")
  pt <- stringr::str_replace_all(x, "[^0-9]", "")
  pt_int <- as.integer(pt)
  pt_int <- pt_int + ifelse(is_pm & pt_int < 1200, 1200, 0)
  pt <- as.character(pt_int)
  pt <- paste0(ifelse(!is.na(pt) & nchar(pt) == 3, "0", ""), pt)
  pt[pt == "NA"] <- NA_character_
  pt
}