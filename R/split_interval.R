#' Split date as character to starts and ends.
#' 
#' @param label target date character.
#' @import magrittr
#' @importFrom lubridate as_date ceiling_date days make_datetime
#' @importFrom Nippon wareki2AD
#' @importFrom purrr as_vector map
#' @importFrom stringi stri_datetime_symbols stri_trans_general
#' @importFrom stringr str_extract str_replace str_split
#' @importFrom tibble data_frame
#' @return tibble. Contains input dates sllit to starts and end days.
#' @examples 
#' \dontrun{
#' split_interval("平成29年3月")
#' split_interval("平成27年4月-5月")
#' split_interval("平成27年4月~5月")
#' split_interval("平成27年4月から5月")
#' split_interval("平成27年4月より5月")
#' 
#' require(lubridate)
#' tibble::data_frame(
#' label = c("平成27年4月~5月",
#'          "平成29年3月",
#'          "平成1年11月から12月",
#'          # [todo: fix] current not working
#'          "平成1年11月から平成12月")
#'          ) %>% 
#'          dplyr::mutate(out = purrr::pmap(., split_interval)) %>% 
#'          tidyr::unnest() %>% 
#'          dplyr::filter(start_date %within% lubridate::interval(ymd("2017-01-01"), 
#'                                            lubridate::ymd("2018-01-01")))
#' }
#' @export
split_interval <- function(label, ...) {
  
  date_label <- stringi::stri_trans_general(label, id = "nfkd")
  
  if (is_interval(date_label) == FALSE) {
    warning("Does not contains interval elements")
  }
  
  fixed_label <- date_label %>% fix_interval_connect_char() %>% 
    fix_interval_complement()
  
  # 平成23年1月-平成23年12月 -> 2000
  # str_split("-")
  # target %>%map(Nippon::wareki2AD)
  
  x <- fixed_label %>%
    stringr::str_replace("(\\u662d\\u548c|\\u5e73\\u6210)[0-9]{1,2}\\u5e74", "") %>% 
    stringr::str_extract(".+\\u6708") %>% 
    stringr::str_split("(-|~|\\u304b\\u3089|\\u3088\\u308a)") %>% 
    purrr::as_vector() %>% 
    purrr::map(
      ~ lubridate::make_datetime(
        stringr::str_extract(date_label, "(\\u662d\\u548c|\\u5e73\\u6210)[0-9]{1,2}\\u5e74") %>% 
          Nippon::wareki2AD(),
        which(stringi::stri_datetime_symbols("ja_JP")$Month == .x),
        1
      )
    )
  
  # Collect last day of month
  x[[length(x)]][1] <- lubridate::ceiling_date(x[[length(x)]][1], unit = "month", change_on_boundary = TRUE) - lubridate::days(1)
  
  if (length(x) == 1) {
    x[[2]] <- x[[1]]
    x[[1]] <- lubridate::floor_date(x[[length(x)]][1], unit = "month")
  }
  
  res <- tibble::data_frame(start_date = lubridate::as_date(x[[1]]), end_date = lubridate::as_date(x[[length(x)]]))
  
  # defunct interval object 
  # to execute purrr::map 
  # res <- res %>% lubridate::as.interval(.[1], .[2])
  
  return(res)
}
