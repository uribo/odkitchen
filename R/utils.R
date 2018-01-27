fix_interval_connect_char <- function(label, ...) {
  
  new_label <- dplyr::case_when(
    stringr::str_detect(label, stringr::fixed("~")) ~ stringr::str_replace_all(label, stringr::fixed("~"), replacement = "-"),
    stringr::str_detect(label, "から") ~ stringr::str_replace_all(label, "から", replacement = "-"),
    stringr::str_detect(label, "より") ~ stringr::str_replace_all(label, "より", replacement = "-")
  )
  
  res <- dplyr::if_else(is.na(new_label) == TRUE, label, new_label)
  
  return(res)
}


.interval_prefix <- function(label) {
  
  elements <- interval_elements()[interval_elements() %in% c(label %>% extract_interval_elements())]
  
  res <- paste0(split_label(label)[[1]], max(elements))
  
  return(res)
}

fix_interval_complement <- function(label) {
  
  elements <- interval_elements()[interval_elements() %in% c(label %>% extract_interval_elements())]
  split_label <- split_label(label)
  
  res <- paste0(paste0(split_label[[1]], max(elements)),
                paste0(split_label[[2]], min(elements)), collapse = "-")
  
  return(res)
}


split_label <- function(label) {
  elements <- interval_elements()[interval_elements() %in% c(label %>% extract_interval_elements())]
  
  res <- label %>% stringr::str_replace_all(paste(elements %>% extract_interval_elements(), collapse = "|"), " ") %>% 
    stringr::str_split("[:space:]", simplify = TRUE) %>% 
    stringr::str_split("-") 
  
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

is_interval <- function(label) {
  
  .is_interval <- function(x) {
    x %>% stringr::str_detect(paste0(paste0("[0-9]{1,99}", interval_elements(), 
                               "-", 
                               "[0-9]{1,99}", interval_elements(), collapse = "|"),
                              paste0(c("昭和", "平成"),
                                     paste0("[0-9]{1,2}", interval_elements()[7], 
                                            "-",
                                            c("昭和", "平成"),
                                            "[0-9]{1,2}", interval_elements()[7], collapse = "|")), collapse = "|"))
  }
  
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

interval_elements <- function() {
  
  # [todo: add] SI units and symbols 
  # [todo: enhance] order
  x <- c("年度", "年", "期", "月", "週", "時", "分", "秒") %>% 
    rev() %>% 
    forcats::fct_inorder(ordered = TRUE)
  
  return(x)
}
