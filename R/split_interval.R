#' Split date as character to starts and ends.
#' 
#' @param label target date character.
#' @import magrittr
#' @importFrom lubridate as_date ceiling_date floor_date days make_datetime ymd
#' @importFrom purrr as_vector keep flatten map
#' @importFrom rlang warn
#' @importFrom stringi stri_datetime_symbols stri_trans_general
#' @importFrom stringr str_extract str_replace str_split
#' @importFrom tibble data_frame
#' @return tibble. Contains input dates sllit to starts and end days.
#' @examples 
#' \dontrun{
#' split_interval("平成29年3月") # Determinate last day of month
#' split_interval("平成27年4月-5月")
#' split_interval("平成27年4~5月")
#' split_interval("平成27年4月から5月")
#' split_interval("平成27年4月より5月")
#' split_interval("平成1年11月から平成1年12月")
#' split_interval("平成20年~平成24年")
#' split_interval("国家公務員退職手当実態調査(平成28年度)")
#' split_interval("平成26年度")
#' split_interval("(平成28年1月~3月)")
#' split_interval("H20.12.31現在")
#' 
#' require(magrittr); require(lubridate)
#' tibble::data_frame(
#' label = c("平成27年4月~5月",
#'          "平成29年3月",
#'          "平成1年11月から12月",
#'          "平成1年11月から平成1年12月")
#'          ) %>% 
#'          dplyr::mutate(out = purrr::pmap(., split_interval)) %>% 
#'          tidyr::unnest() %>% 
#'          dplyr::filter(start_date %within% lubridate::interval(ymd("2017-01-01"), 
#'                                            lubridate::ymd("2018-01-01")))
#' }
#' @export
split_interval <- function(label, ...) {
  
  fixed_label <- stringi::stri_trans_general(label, id = "nfkd") %>% 
    fix_interval_connect_char()
  
  extract_jyr_doubt <- fixed_label %>% 
    stringr::str_extract("\\(.+\\)") %>% 
    stringr::str_replace_all(stringr::regex("(\\(|\\[|\\]|\\))"), "")

  if (is.na(extract_jyr_doubt) == FALSE)
    if (extract_jyr_doubt %>% is_jyr() == TRUE) fixed_label <- extract_jyr_doubt
  
  x <- fixed_label %>% convert_jyr()
  
  # Not jyr label
  if (is.na(x) == TRUE) {
    res <- tibble::data_frame(start_date = NA, end_date = NA)
  } else {
    # TODO: enhance fiscal year
    if (fixed_label %>% stringr::str_detect("[0-9]{1,2}年度") == TRUE) {
      # x <- fixed_label %>% convert_jyr()
      res <- tibble::data_frame(start_date = lubridate::make_date(x, 4, 1), end_date = lubridate::make_date(x + 1, 4, 1) - 1)
    } else {
      if (.is_interval(fixed_label) == FALSE) {
        fixed_label_tmp <- fixed_label %>%
          fix_interval_complement() %>% 
          stringr::str_split("-") 
    
        if (length(fixed_label_tmp[[1]]) == 1) {
          x <- fixed_label %>%
            stringr::str_split("-") %>%
            purrr::map(~ convert_jyr_date(.x) %>% as.character) %>% 
            purrr::flatten()
        } else {
          x <- suppressWarnings(fixed_label_tmp %>% 
                                  purrr::flatten() %>% 
                                  purrr::map(parse_jyd)
                                # %>% 
                                #   purrr::map(~ convert_jyr_date(.x) %>% as.character) %>% 
                                #   purrr::flatten()
                                )
        }
        
        if (stringr::str_detect(x[[length(x)]], "1$") == TRUE) x[[length(x)]][1]  <- as.character(lubridate::ceiling_date(lubridate::ymd(x[[length(x)]]), unit = "month", change_on_boundary = TRUE) - lubridate::days(1))
          
        
        if (length(x) == 1) {
          rlang::warn("Does not contains interval elements")
          x[[2]] <- x[[1]]
          x[[1]] <- as.character(lubridate::floor_date(lubridate::ymd(x[[length(x)]][1]), unit = "month"))
        }
        
      } else {
        
        xx <- suppressWarnings(fixed_label %>% 
                                 stringr::str_extract_all(pattern = paste0(paste0("(", jyr %>% paste0("|西暦[0-9]{1,4}|[0-9]{1,4}年"), ")"),
                                                                           "|[0-9]{1,2}月|[0-9]{1,2}日")) %>% 
                                 purrr::flatten() %>% 
                                 purrr::keep(~ .x %>% stringr::str_detect(pattern = paste0("^(", jyr %>% paste0("|西暦[0-9]{1,4}|[0-9]{1,4}年"), ")"))))
        
        # fix: YY-YY
        if (length(xx) == 2) {
          x <- xx %>% purrr::map(~ convert_jyr_date(.x) %>% as.character) %>% 
            purrr::flatten()
          
          x[[1]][1] <- as.character(lubridate::make_date(lubridate::year(x[[1]][1]), lubridate::month(x[[1]][1]) - 11, lubridate::day(x[[1]][1])))
          
          x[[length(x)]][1]  <- as.character(lubridate::ceiling_date(lubridate::ymd(x[[length(x)]]), unit = "month", change_on_boundary = TRUE) - lubridate::days(1))
        } else {
          x <- fixed_label %>%
            stringr::str_replace("(\\u662d\\u548c|\\u5e73\\u6210)[0-9]{1,2}\\u5e74", "") %>% 
            stringr::str_extract(".+\\u6708") %>% 
            stringr::str_split("(-|~|\\u304b\\u3089|\\u3088\\u308a)") %>% 
            purrr::as_vector() %>% 
            purrr::map(
              ~ lubridate::make_datetime(
                stringr::str_extract(fixed_label, "(\\u662d\\u548c|\\u5e73\\u6210)[0-9]{1,2}\\u5e74") %>% 
                  convert_jyr(),
                which(stringi::stri_datetime_symbols("ja_JP")$Month == .x),
                1
              )
            ) 
          # Collect last day of month
          x[[length(x)]][1] <- lubridate::ceiling_date(x[[length(x)]][1], unit = "month", change_on_boundary = TRUE) - lubridate::days(1)
        }
        
      }
      
      if (length(x) == 1) {
        x[[2]] <- x[[1]]
        x[[1]] <- lubridate::floor_date(x[[length(x)]][1], unit = "month")
      }
      
      res <- tibble::data_frame(start_date = lubridate::as_date(x[[1]]), end_date = lubridate::as_date(x[[length(x)]]))
      
    }    
  }
    
  return(res)
}
