#' Convert Japanese imperial year to Anno Domini
#' 
#' @param jyr Japanese imperial year. kanji or roman character
#' @importFrom dplyr case_when
#' @importFrom glue collapse
#' @importFrom purrr as_vector
#' @importFrom rlang warn
#' @importFrom stringi stri_datetime_symbols
#' @importFrom stringr str_detect str_extract str_sub
#' 
#' @references Nippon wareki2AD
#' @example 
#' convert_jyr("H2")
#' convert_jyr("平成14年")
#' @export
convert_jyr <- function(jyr) {
  
  # TODO: round option
  
  jyr <- stringr::str_trim(jyr)
  
  if (is_jyr(jyr) == FALSE) rlang::warn("Unsupported Japanese imperial year.")
  
  if (stringr::str_detect(jyr, "[A-Z]") == TRUE) jyr <- stringr::str_to_lower(jyr)
  # 
  # jyr_kanji <- stringi::stri_datetime_symbols(locale = "ja_JP_TRADITIONAL")$Era[233:236]
  # jyr_kanki_capital <- jyr_kanji %>% str_sub(1, 1)
  # jyr_sets <- list(
  #   meiji = c(jyr_kanji[1], jyr_kanki_capital[1], "meiji", "m"),
  #   taisyo = c(jyr_kanji[2], jyr_kanki_capital[2], "taisyo", "taisho", "taisyou", "t"),
  #   syouwa = c(jyr_kanji[3], jyr_kanki_capital[3], "syouwa", "showa", "s"),
  #   heisei = c(jyr_kanji[4], jyr_kanki_capital[4], "heisei", "h"))
  # 
  # if (stringr::str_detect(jyr, 
  #                         paste0("^(", glue::collapse(jyr_sets %>% purrr::as_vector(), sep = "|"), ")")) == FALSE) {
  #   
  #   
  #   
  # }
  
  wareki_yr <- jyr %>% 
    stringr::str_extract(pattern = "[0-9]{1,2}") %>% as.integer()
  
  jyr <- jyr %>% 
    stringr::str_sub(1, 1)
  
  wareki <- dplyr::case_when(
    jyr %in% jyr_sets[[1]] ~ names(jyr_sets[1]),
    jyr %in% jyr_sets[[2]] ~ names(jyr_sets[2]),
    jyr %in% jyr_sets[[3]] ~ names(jyr_sets[3]),
    jyr %in% jyr_sets[[4]] ~ names(jyr_sets[4]))
  
  
  res <- dplyr::case_when(
    wareki == names(jyr_sets[1]) ~ wareki_yr + 1867,
    wareki == names(jyr_sets[2]) ~ wareki_yr + 1911,
    wareki == names(jyr_sets[3]) ~ wareki_yr + 1925,
    wareki == names(jyr_sets[4]) ~ wareki_yr + 1988
  )
  
  return(res)    
}

jyr_kanji <- stringi::stri_datetime_symbols(locale = "ja_JP_TRADITIONAL")$Era[233:236]
jyr_kanki_capital <- jyr_kanji %>% stringr::str_sub(1, 1)
jyr_sets <- list(
  meiji = c(jyr_kanji[1], jyr_kanki_capital[1], "meiji", "m"),
  taisyo = c(jyr_kanji[2], jyr_kanki_capital[2], "taisyo", "taisho", "taisyou", "t"),
  syouwa = c(jyr_kanji[3], jyr_kanki_capital[3], "syouwa", "showa", "s"),
  heisei = c(jyr_kanji[4], jyr_kanki_capital[4], "heisei", "h"))

is_jyr <- function(jyr) {
  if (stringr::str_detect(jyr, "[A-Z]") == TRUE) jyr <- stringr::str_to_lower(jyr)

  if (stringr::str_detect(jyr, 
                          paste0("^(", glue::collapse(jyr_sets %>% purrr::as_vector(), sep = "|"), ")")) == FALSE) {
    
    FALSE
    # rlang::abort("Unsupported Japanese imperial year.")
  } else {
    TRUE  
  }
}

convert_jyr_date <- function(x) {
  
  element_yr <- dplyr::if_else(x %>% stringr::str_detect(pattern = paste0("^(", jyr %>% paste0("|西暦[0-9]{1,4}|[0-9]{1,4}年"), ")")),
                               x %>% stringr::str_extract(pattern = paste0("^(", jyr %>% paste0("|西暦[0-9]{1,4}|[0-9]{1,4}年"), ")")),
                               NA_character_) %>% convert_jyr()
  element_mo <- dplyr::if_else(x %>% stringr::str_detect(pattern = "[0-9]{1,2}月"),
                               x %>% stringr::str_extract(pattern = "[0-9]{1,2}月"),
                               "12月") %>% readr::parse_number()
  element_dd <- dplyr::if_else(x %>% stringr::str_detect(pattern = "[0-9]{1,2}日"),
                               x %>% stringr::str_extract(pattern = "[0-9]{1,2}日"),
                               "1日") %>% readr::parse_number()
  res <- list(year = element_yr, month = element_mo, day = element_dd)
  
  res <- lubridate::make_date(res$year, res$month, res$day)  
  
  return(res)
}
