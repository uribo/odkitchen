# library(odkitchen); 
library(magrittr)
library(tidyverse)
library(testthat)
library(rvest)
library(glue)

base_url <- "https://www.e-stat.go.jp/stat-search/files"
x <- read_html(glue::glue(
  base_url,
  "?toukei=00130001&",
  "page={page}",
  page = 1
))
max_page <- x %>% 
  html_nodes(css = 'div.stat-paginate-list.js-page_current.lef > span.stat-paginate-last') %>% 
  html_attr("data-page") %>% 
  unique() %>% as.integer() %>% 
  max()

read_crime_list <- function(page = 1) {
  
  page_counter <- page
  
  base_url <- "https://www.e-stat.go.jp/stat-search/files"
  x <- read_html(glue::glue(
    base_url,
    "?toukei=00130001&",
    "page={page_counter}"
  ))
  x %>% 
    html_nodes(css = 'div.stat-paginate-list.js-page_current.lef > span.stat-paginate-last') %>% 
    html_attr("data-page") %>% 
    unique() %>% as.integer() %>% 
    max()
  char_stats_title <- x %>% html_nodes(css = 'div.stat-search_result-header > span.stat-title') %>% 
    html_text(trim = TRUE)
  stats_table_elements <- x %>% html_nodes(css = 'div.stat-search_result-list.js-items > a > ul > li > div')
  
  xx <- stats_table_elements %>% 
    html_nodes(css = "span.stat-title > span")
  
  df_stats <- tibble::tibble(
    catalog = char_stats_title,
    published_date = stats_table_elements %>%
      html_nodes(css = "span.stat-date") %>%
      html_text(trim = TRUE),
    title = xx %>% html_text(trim = TRUE) %>% 
      purrr::map_chr(~ stringr::str_replace_all(.x, "\\[.+\\]", "")),
    url = xx %>%
      purrr::map_chr(
        ~ glue::glue(base_url,
                     "?toukei=00130001&",
                     "page={page_counter}",
                     "&",
                     .x %>% html_attr("data-key") %>% .[[1]],
                     "=",
                     .x %>% html_attr("data-value") %>% .[[1]],
                     page_counter = 1)
      ),
    items = xx %>%
      purrr::map_chr(~ stringr::str_extract(.x, pattern = "\\[.+\\]") %>%
                       readr::parse_number())
  ) %>% readr::type_convert() %>% 
    # [reference] stringi::stri_datetime_symbols(locale = "ja_JP_Traditional")$Era,
    # [reference] stringi::stri_datetime_symbols("@calendar=japanese")
    mutate_if(is.character, stringi::stri_trans_general, id = "nfkd") %>% 
    mutate(out = purrr::pmap(., ~ split_interval(..3))) %>%
    unnest() %>%
    group_by(title, start_date, end_date) %>%
    nest() %>%
    rename(information = data)
  
  # defunct
    # mutate(
    #   fiscal_year = str_extract(title, "(昭和|平成)[0-9]{1,2}年") %>% 
    #     Nippon::wareki2AD())
  
  return(df_stats)
}

expect_equal(
  nrow(read_crime_list(1)),
  50L
)

df_stats <- 1:max_page %>% 
  purrr::map_df(read_crime_list)

expect_gte(
  nrow(df_stats),
  120
)
expect_named(
  df_stats,
  c("title", "start_date", "end_date", "information")
)
expect_named(
  df_stats %>% unnest(),
  c("title", "start_date", "end_date", "catalog", "published_date", "url", "items")
)
expect_equal(
  nrow(df_stats %>% unnest() %>% 
         filter(is.na(start_date))),
  0L
)

df_stats %>% readr::write_rds(here::here("data-raw", "catalog_crime.rds"))
