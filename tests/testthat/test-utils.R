context("package utilities")

test_that("misc", {
  
  expect_equal(
    fix_interval_connect_char("1月-6月"),
    "1月-6月")
  expect_equal(
    "平成22年1年~4月犯罪統計" %>% 
      fix_interval_connect_char(),
    "平成22年1年-4月犯罪統計"
  )
  expect_equal(
    c("3月~6月", "2月から4月", "1月より2月") %>% 
      purrr::map_chr(fix_interval_connect_char),
    c("3月-6月", "2月-4月", "1月-2月")
  )
  
  expect_true(
    is_interval("1月-3月")  
  )
  #[todo: discussion]strict true?
  expect_false(
    is_interval("平成22年1-2月aaaa")
  )
  #[todo: discussion]strict true?
  expect_false(
    is_interval("平成22年1月-平成23")
  )
  expect_equal(
    c("3月~6月", "2月から4月", "1月より2", "平成23年1~12月", "平成23年1月") %>% 
      purrr::map_lgl(is_interval),
    c(TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  
})

test_that("vector", {
  
  expect_length(interval_elements(), 9L)
  expect_true(
    interval_elements()[1] < interval_elements()[3]  
  )
  
})
