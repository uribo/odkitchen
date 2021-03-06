context("split interval elements")

test_that("validation", {
  
  expect_warning(
    split_interval("平成29年3月"), 
                   'Does not contains interval elements'
  )
  expect_equal(
    suppressWarnings(split_interval("平成29年3月")),
    tibble::data_frame(start_date = lubridate::ymd("2017-03-01"), 
                       end_date = lubridate::ymd("2017-03-31"))
  )
  
  expect_s3_class(
    split_interval("平成27年4月-5月"),
    "tbl_df"
  )
  expect_equal(
    split_interval("平成27年4月-5月"),
    tibble::data_frame(start_date = lubridate::ymd("2015-04-01"), 
                       end_date = lubridate::ymd("2015-05-31"))
  )
  expect_equal(
    split_interval("平成27年4~5月"),
    tibble::data_frame(start_date = lubridate::ymd("2015-04-01"), 
                       end_date = lubridate::ymd("2015-05-31"))
  )
  expect_equal(
    split_interval("平成27年4月から5月"),
    split_interval("平成27年4月より5月")
  )
  
  expect_equal(
    split_interval("平成1年11月から平成1年12月"),
    tibble::data_frame(start_date = lubridate::ymd("1989-11-01"), 
                       end_date = lubridate::ymd("1989-12-31"))
  )
  expect_equivalent(
    split_interval("平成20年~平成24年")$start_date,
    lubridate::ymd("2008-01-01"))
  
  expect_equal(
    split_interval("国家公務員退職手当実態調査(平成28年度)"),
    tibble::data_frame(start_date = lubridate::ymd("2016-04-01"), 
                       end_date = lubridate::ymd("2017-03-31"))
  )
  expect_equal(
    split_interval("平成26年度"),
    tibble::data_frame(start_date = lubridate::ymd("2014-04-01"), 
                       end_date = lubridate::ymd("2015-03-31"))
  )
  expect_equal(
    split_interval("(平成28年1月~3月)"),
    tibble::data_frame(start_date = lubridate::ymd("2016-01-01"), 
                       end_date = lubridate::ymd("2016-03-31"))
  )
  expect_equal(
    split_interval("H20.12.31現在"),
    tibble::data_frame(start_date = lubridate::ymd("2008-12-31"), 
                       end_date = lubridate::ymd("2008-12-31"))
  )
  expect_equal(
    split_interval("平成17年(2005年)"),
    tibble::data_frame(start_date = lubridate::ymd("2005-01-01"), 
                       end_date = lubridate::ymd("2005-12-31"))
  )
  expect_equal(
    split_interval("平成7年"),
    tibble::data_frame(start_date = lubridate::ymd("1995-01-01"), 
                       end_date = lubridate::ymd("1995-12-31"))
  )
  
})
