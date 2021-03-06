fix_interval_connect_char <- function(label, ...) {
  
  new_label <- dplyr::case_when(
    stringr::str_detect(label, stringr::fixed("~")) ~ stringr::str_replace_all(label, stringr::fixed("~"), replacement = "-"),
    stringr::str_detect(label, "から") ~ stringr::str_replace_all(label, "から", replacement = "-"),
    stringr::str_detect(label, "より") ~ stringr::str_replace_all(label, "より", replacement = "-")
  )
  
  res <- dplyr::if_else(is.na(new_label) == TRUE, label, new_label)
  
  return(res)
}


# c("平成23年-平成25年") %>% .interval_prefix()
.interval_prefix <- function(label) {
  
  elements <- interval_elements()[interval_elements() %in% c(label %>% extract_interval_elements())]
  
  res <- paste0(split_label(label)[[1]], max(elements))
  
  return(res)
}

fix_interval_complement <- function(label) {
  
  elements <- interval_elements()[interval_elements() %in% c(label %>% 
                                                               stringr::str_replace("現在", "") %>% 
                                                               extract_interval_elements())]
  
  if (length(elements) != 0) {
    splited_label <- split_label(label)
    
    # TODO: enhanced
    if (length(splited_label) == 1 & length(elements) == 1) {
      res <- paste(paste0(splited_label[[1]], elements),
                   paste0(splited_label[[1]], elements), sep = "-")
    } else if (length(splited_label) == 2 & length(elements) == 1) {
      res <- paste(paste0(splited_label[[1]], elements),
                   paste0(splited_label[[2]], elements))
    } else if (length(splited_label) == 2 & length(elements) == 2) {
      res <- paste0(paste0(splited_label[[1]], max(elements)),
                    paste0(splited_label[[2]], min(elements)), collapse = "-")
    } else if (length(splited_label) == 3) {
      res <- paste0(paste0(splited_label[[1]], elements[3]),
                    paste0(splited_label[[2]], elements[2]),
                    paste0(splited_label[[3]], elements[1]), collapse = "-")
    } else if (length(splited_label) == 4) {
      res <- paste0(paste0(splited_label[[1]], elements[2]),
                    paste0(splited_label[[2]], elements[1]),
                    paste0(splited_label[[3]], elements[2]),
                    paste0(splited_label[[4]], elements[1]), collapse = "-")
    } else if (length(splited_label) > 5) {
      res <- paste(paste0(splited_label[[1]], max(elements)),
                   paste0(splited_label[[1]], max(elements)))
    }
        
  } else {
    res <- paste(label, label, sep = "-")
  }
  
  return(res)
}

split_label <- function(label) {
  elements <- interval_elements()[interval_elements() %in% c(label %>% extract_interval_elements())]
  
  # tmp_res <- label %>% 
  #   stringr::str_split_fixed(paste(elements %>% extract_interval_elements(), collapse = "|"), n = length(elements) + 1) %>% 
  #   .[1:length(elements)]
    
  tmp_res <- label %>%
    stringr::str_replace_all(paste(elements %>% extract_interval_elements(), collapse = "|"), " ") %>%
    stringr::str_split("[:space:]", simplify = TRUE) %>%
    stringr::str_extract("[:print:]{1,99}") %>%
    na.omit()
  attributes(tmp_res) <- NULL
  
  res <- tmp_res %>% stringr::str_split("-") 

  checked_empty <- res %>% purrr::map(stringi::stri_isempty)
   
  if (checked_empty %>% purrr::flatten_lgl() %>% sum() >= 1) {
    res <- tmp_res %>% 
      as.list()
  }
  
  # purrr::discard(~ stringi::stri_isempty(.x) == TRUE) %>% 
  #   
  #   purrr::flatten() %>% 
  #   purrr::discard(~ stringi::stri_isempty(.x) == TRUE)
  # 
      
  return(res)
}

fix_interval_jyear <- function(label) {
  xx <- interval_elements()[interval_elements() %in% c(label %>% extract_interval_elements())] %>% 
    as.character()
  
  xxx <- label %>% 
    str_extract(paste0(stringi::stri_datetime_symbols("ja_JP_TRADITIONAL")$Era, collapse = "|"))
  
  res <- str_extract(label, paste0(xxx, ".+", xx, "-.+", xx))  
  
  return(res)
}

.is_interval <- function(x) {
  
  
  check <- x %>% stringr::str_detect(paste0(paste0("[0-9]{1,99}", interval_elements(), 
                                          "-", 
                                          "[0-9]{1,99}", interval_elements(), collapse = "|"),
                                   paste0(c("昭和", "平成"),
                                          paste0("[0-9]{1,2}", interval_elements()[7], 
                                                 "-",
                                                 c("昭和", "平成"),
                                                 "[0-9]{1,2}", interval_elements()[7], collapse = "|")), collapse = "|"))
  
  # res <- if_else(check == FALSE, stringr::str_detect(x, "-"), check)
  
  return(check)
}

is_interval <- function(label) {
  
  label <- fix_interval_connect_char(label)
  
  res <- label %>% .is_interval()
    
  if (res == FALSE) {
    res <- fix_interval_complement(label) %>% 
      stringr::str_replace_all(.interval_prefix(label), "") %>% 
      .is_interval()
  }
  
  return(res)
}

extract_interval_elements <- function(label) {
  elements_candidate <- label %>% 
    stringr::str_replace_all("[0-9]", "") %>% 
    stringr::str_split_fixed("-", n = 2)
  
  elements_candidate <- elements_candidate %>% 
    # [todo: fix] nendo
    stringi::stri_split_boundaries(type = "character", simplify = TRUE) %>% c()
  
  # elements_candidate <- map2_chr(
  #   .x = elements_candidate,
  #   .y = c(-1, 1),
  #   .f = ~ str_sub(.x, start = .y, end = .y)
  # )
  
  elements_candidate <- elements_candidate[elements_candidate %in% interval_elements()]
  
  # # [todo: enhance] if multiple?
  # if ((elements_candidate[1] == elements_candidate[2]) == FALSE) {
  #   x2 <- label %>% extract_interval_elements()
  #   
  #   if ((x[x %in% x2[1]] > x[x %in% x2[2]]) == TRUE) {
  #     elements_candidate <- x2[2]
  #   } else {
  #     elements_candidate <- x2[1]
  #   }
  # }
  
  res <- unique(elements_candidate)
  
  return(res)
}

parse_jyd <- function(date) {
  # [consider] use lubridate::parse_date_time?
  yy <- suppressWarnings(date %>% convert_jyr())
  
  if (is.na(yy) == FALSE) {
      # if (stringr::str_detect(date, "([0-9]{1,2}|[0-9]{4})年$") == TRUE) {
    #   x
    # }
    x <- date %>% 
      stringr::str_trim() %>% 
      stringr::str_replace("現在", "") %>% 
      stringr::str_split(paste0("(\\.|-|",
                                paste(interval_elements(), collapse = "|"),
                                ")"), simplify = TRUE) %>% 
      as.vector() %>% 
      purrr::discard(~ stringi::stri_isempty(.x) == TRUE) %>% 
      purrr::keep(~ stringr::str_detect(.x, "[0-9]{1,4}$") == TRUE)
    
    mm <- stringr::str_extract(dplyr::if_else(is.na(x[2]), "01", x[2]), "[0-9]{1,2}")
    dd <- stringr::str_extract(dplyr::if_else(is.na(x[3]), "01", x[3]), "[0-9]{1,2}")
    
    res <- lubridate::make_date(yy, mm, dd)
    
    return(res)
  }
  return(date)
}

interval_elements <- function() {
  
  # TODO: SI units and symbols 
  # TODO: order
  x <- c("年度", "年", "期", "月", "週", "日", "時", "分", "秒") %>% 
    rev() %>% 
    forcats::fct_inorder(ordered = TRUE)
  
  return(x)
}

jyr_kanji <- stringi::stri_datetime_symbols(locale = "ja_JP_TRADITIONAL")$Era[233:236]
jyr_kanki_capital <- jyr_kanji %>% stringr::str_sub(1, 1)
jyr_roman_capital <- c("m", "t", "s", "h")
jyr_sets <- list(
  meiji = c(jyr_kanji[1], jyr_kanki_capital[1], "meiji", jyr_roman_capital[1]),
  taisyo = c(jyr_kanji[2], jyr_kanki_capital[2], "taisyo", "taisho", "taisyou", jyr_roman_capital[2]),
  syouwa = c(jyr_kanji[3], jyr_kanki_capital[3], "syouwa", "showa", jyr_roman_capital[3]),
  heisei = c(jyr_kanji[4], jyr_kanki_capital[4], "heisei", jyr_roman_capital[4]))
# nen?
jyr <- paste0(c(jyr_kanji, jyr_roman_capital),
              "[0-9]{1,2}", collapse = "|")
