fix_interval_char <- function(label, ...) {
  
  new_label <- dplyr::case_when(
    stringr::str_detect(label, stringr::fixed("~")) ~ stringr::str_replace_all(label, stringr::fixed("~"), replacement = "-"),
    stringr::str_detect(label, "から") ~ stringr::str_replace_all(label, "から", replacement = "-"),
    stringr::str_detect(label, "より") ~ stringr::str_replace_all(label, "より", replacement = "-")
  )
  
  res <- dplyr::if_else(is.na(new_label) == TRUE, label, new_label)
  
  return(res)
}

is_interval <- function(label) {
  res <- fix_interval_char(label) %>% 
    stringr::str_detect(paste0("[0-9]{1,99}", interval_elements(), 
                      "-", 
                      "[0-9]{1,99}", interval_elements(), collapse = "|"))
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
