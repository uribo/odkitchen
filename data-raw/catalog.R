devtools::load_all(".")
library(magrittr)
library(testthat)

# ファイルから探す ----------------------------------------------------------------
x <- read_html("https://www.e-stat.go.jp/stat-search/files?page=1")
max_page <- x %>% html_nodes(css = 'div.stat-paginate-list.js-page_current.lef > span.stat-paginate-last') %>% 
  html_attr("data-page") %>% 
  unique() %>% as.integer()

df_stats_files <- 1:max_page %>% 
  purrr::map_dfr(collect_stats_list, source = "files")
expect_equal(
  dim(df_stats_files),
  c(615, 2)
)
df_stats_files %>% 
  write_rds(here::here("data-raw", "catalog_list_files.rds"))

df_stats_files <- readr::read_rds(here::here("data-raw", "catalog_list_files.rds"))
expect_length(df_stats_files$page_url, 615L)
# safe... 1:75
df_all <- df_stats_files$page_url %>%
  purrr::map_dfr(function(.x) {
    # nest = FALSE
    collect_stats_catalog(.x)
  })
expect_equal(nrow(df_all), 1642)
# df_all %>% readr::write_rds(here::here("data-raw", "catalog_title_list.rds"))

df_all <- readr::read_rds(here::here("data-raw", "catalog_title_list.rds"))
catalog <- df_all %>% 
  dplyr::mutate(out = purrr::pmap(., ~ split_interval(..6))) %>% 
  tidyr::unnest() %>% 
  dplyr::select(-omit_title) %>% 
  dplyr::group_by(title, published_date) %>% 
  tidyr::nest() %>%
  dplyr::rename(information = data)

expect_equal(
  catalog$title %>% stringr::str_detect("新着") %>% sum(),
  0L
)

expect_equal(
  dim(catalog),
  c(1606, 3)
)
expect_equal(
  dim(tidyr::unnest(catalog)),
  c(1642, 7)
)

expect_named(
  catalog,
  c("title", "published_date", "information")
)

expect_named(
  catalog %>% tidyr::unnest(),
  c("title", "published_date", "catalog", "url", "items", "start_date", "end_date")
)
expect_equal(
  nrow(catalog %>% tidyr::unnest() %>%
         dplyr::filter(is.na(start_date))),
  757L
)

usethis::use_data(catalog, overwrite = TRUE)
