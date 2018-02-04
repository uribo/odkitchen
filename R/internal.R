collect_stats_catalog <- function(url) {
  Sys.sleep(rnorm(1, mean = 4))
  toukei <- url %>% 
    stringr::str_extract("toukei=[0-9]{1,30}") %>% 
    stringr::str_replace("toukei=", "")
  
  max_page <- guess_max_page(toukei)
  
  catalog <- 1:max_page %>% 
    purrr::map_dfr(~ read_stats_fileslist(toukei = toukei, .x, nest = FALSE))
  
  return(catalog)
}

read_stats_fileslist <- function(toukei, page = 1, nest = TRUE) {
  
  base_url <- "https://www.e-stat.go.jp/stat-search/files"
  x <- xml2::read_html(glue::glue(
    base_url,
    "?toukei={toukei}&",
    "page={page}"
  ))
  
  char_stats_title <- x %>% rvest::html_nodes(css = 'div.stat-search_result-header > span.stat-title') %>% 
    rvest::html_text(trim = TRUE)
  stats_table_elements <- x %>% rvest::html_nodes(css = 'div.stat-search_result-list.js-items > a > ul > li > div')
  
  xx <- stats_table_elements %>% 
    rvest::html_nodes(css = "span.stat-title > span")
  
  df_stats <- tibble::tibble(
    catalog = char_stats_title,
    published_date = stats_table_elements %>%
      rvest::html_nodes(css = "span.stat-date") %>%
      rvest::html_text(trim = TRUE),
    title = xx %>% rvest::html_text(trim = TRUE) %>% 
      purrr::map_chr(~ stringr::str_replace_all(.x, "\\[.+\\]", "")),
    url = xx %>%
      purrr::map_chr(
        ~ glue::glue(base_url,
                     "?toukei={toukei}&",
                     "page={page}",
                     "&",
                     .x %>% rvest::html_attr("data-key") %>% .[[1]],
                     "=",
                     .x %>% rvest::html_attr("data-value") %>% .[[1]])
      ),
    items = xx %>%
      purrr::map_chr(~ stringr::str_extract(.x, pattern = "\\[.+\\]") %>%
                       readr::parse_number())
  ) %>% 
    readr::type_convert() %>% 
    dplyr::mutate_if(is.character, stringi::stri_trans_general, id = "nfkd") %>% 
    dplyr::mutate(omit_title = omit_redundant_label(title))
  
  if (nest == FALSE) {
    return(df_stats)
  } else if (nrow(df_stats) > 1) {
    df_stats <- df_stats %>% 
      stats_seprate_interval() %>% 
      stats_nest()
  }

  return(df_stats)
}

omit_redundant_label <- function(label) {
  
  res <- stringr::str_replace_all(label, paste("-活動調査", "-基礎調査", "国勢調査 遡及集計.+", sep = "|"), "") 
  res <- stringr::str_replace_all(res, "平成2-7-12年接続", "平成2年-平成12年接続")
  res <- stringr::str_replace_all(res, "平成7-12-17年接続", "平成7年-平成17年接続")
  res <- stringr::str_replace_all(res, "昭和60年-平成2-7年接続", "昭和60年-平成7年接続")
  res <- stringr::str_replace_all(res, "平成12-17-23年接続", "平成12年-平成23年接続")
  res <- stringr::str_replace_all(res, "\\([0-9]{1,4}年\\)産業連関構造調査", "産業連関構造調査")
  res <- stringr::str_replace_all(res, "平成10~14年", "平成10年-平成14年")
  res <- stringr::str_replace_all(res, "平成15~19年", "平成15年-平成19年")
  res <- stringr::str_replace_all(res, "平成20~24年", "平成20年-平成24年")
  res <- stringr::str_replace_all(res, "※.+", "")
  res <- stringr::str_replace_all(res, "第[0-9]{1,2}回 平成[0-9]{1,2}年\\([0-9]{4}年\\)外資系企業動向調査", "")
  res <- stringr::str_replace_all(res, "「公共土木施設水害統計調査」は平成22年より「水害統計調査」に統合しました。", "")
  return(res)
}

unique_jyr_label <- function(label) {
  jyrs <- extract_jyr(label)
  
  if (length(jyrs) > 1) {
    if (identical_jyr(jyrs[1], jyrs[2]) == TRUE) {
      label <- stringr::str_replace(label, as.character(unique_jyr(jyrs)), "")
    }
  }
  return(label)
}

extract_jyr <- function(x) {
  split_jyr_element(x) %>% 
    purrr::keep(~ stringr::str_detect(.x, "年$") == TRUE)
}

identical_jyr <- function(x, y) {
  
  check <- c(x, y) %>% purrr::map(convert_jyr) %>% 
    purrr::map(convert_jyr) %>% 
    unique() %>% 
    purrr::flatten_dbl() %>% 
    length()
  
  dplyr::if_else(check == 1L, TRUE, FALSE)
  
}

unique_jyr <- function(x) {
  
  jyrs <- extract_jyr(x)
  
  res <- jyrs %>% 
    purrr::map(convert_jyr) %>% 
    unique() %>% 
    purrr::flatten_dbl()
  
  return(res)
}

split_jyr_element <- function(x) {
  x %>% 
    stringr::str_split(stringr::regex("\\(|\\)"), simplify = TRUE)
}


stats_seprate_interval <- function(data) {
  
  res <- data %>% 
    dplyr::mutate(out = purrr::pmap(., ~ odkitchen::split_interval(..3))) %>%
    # unnest date columns
    tidyr::unnest()
  
  return(res)
}

stats_nest <- function(data) {
  
  res <- data %>% 
    dplyr::group_by(title, published_date) %>% 
    tidyr::nest() %>%
    dplyr::rename(information = data) 
  
  return(res)
}

guess_max_page <- function(toukei) {
  
  tmp_res <- glue::glue(
    "https://www.e-stat.go.jp/stat-search/files?page=1&toukei={toukei}&result_page=1"
  ) %>% 
    xml2::read_html() %>% 
    rvest::html_nodes(css = 'div.stat-paginate-list.js-page_current.lef > span.stat-paginate-last') %>% 
    rvest::html_attr("data-page")
  
  if (length(tmp_res) == 0) {
    res <- 1L
  } else {
    res <- tmp_res %>% unique() %>% max() %>% as.integer() 
  }
  
  return(res)  
}
