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
#' @examples
#' convert_jyr("H2")
#' convert_jyr("平成14年")
#' @export
convert_jyr <- function(jyr) {
  
  # TODO: round option
  jyr <- sapply(jyr, stringr::str_trim, USE.NAMES = FALSE)
  
  if (sum(sapply(jyr, stringr::str_detect, pattern = "([0-9]{4}|[0-9]{4}.+年)", USE.NAMES = FALSE)) > 0)
    return(as.numeric(stringr::str_replace(jyr, "年", "")))
    
  if (sum(is_jyr(jyr)) == 0) 
    rlang::warn("Unsupported Japanese imperial year.")
  
  jyr <- 
    jyr_initial_tolower(jyr)
  
  wareki_yr <- 
    jyr %>% 
    stringr::str_extract(pattern = "[0-9]{1,2}") %>% as.integer()
  
  jyr <- 
    stringr::str_sub(jyr, 1, 1)
  
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

jyr_initial_tolower <- function(jyr) {
  purrr::modify_if(
      jyr,
      .p = ~ stringr::str_detect(.x, "[A-Za-z]"),
      stringr::str_to_lower)
}

is_jyr <- function(jyr) {
  
  jyr_initial_tolower(jyr) %>% 
    purrr::map_lgl(
      ~ stringr::str_detect(.x, 
                            paste0("^(", glue::glue_collapse(jyr_sets %>% 
                                                               purrr::as_vector(), 
                                                             sep = "|"), ")"))
    )
}

convert_jyr_date <- function(x) {
  
  elements <- interval_elements()[interval_elements() %in% c(x %>% extract_interval_elements())]

  element_yr <- dplyr::if_else(x %>% stringr::str_detect(pattern = paste0("^(", jyr %>% paste0("|西暦[0-9]{1,4}|[0-9]{1,4}年"), ")")),
                               x %>% stringr::str_extract(pattern = paste0("^(", jyr %>% paste0("|西暦[0-9]{1,4}|[0-9]{1,4}年"), ")")),
                               NA_character_) %>% convert_jyr()

  if (length(elements) <= 1) {
    res <- x %>% parse_jyd()
  } else {
    element_mo <- dplyr::if_else(x %>% stringr::str_detect(pattern = "[0-9]{1,2}月"),
                                 x %>% stringr::str_extract(pattern = "[0-9]{1,2}月"),
                                 "12月") %>% readr::parse_number()
    element_dd <- dplyr::if_else(x %>% stringr::str_detect(pattern = "[0-9]{1,2}日"),
                                 x %>% stringr::str_extract(pattern = "[0-9]{1,2}日"),
                                 "1日") %>% readr::parse_number()
    res <- list(year = element_yr, month = element_mo, day = element_dd)
    
    res <- lubridate::make_date(res$year, res$month, res$day)      
  }
   


  
  return(res)
}
