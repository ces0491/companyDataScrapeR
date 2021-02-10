test_that("get data from Yahoo Finance", {

  tickers <- c("TSLA", "CLS.JO")
  type_fs <- c("IS", "BS", "CFS")

  start_date <- "2020/07/01"
  end_date <- "2020/07/31"
  frequency <- "weekly"

  # test financials

  test_financials <- get_yahoo_data(tickers, type_fs)

  testthat::expect_s3_class(test_financials, "tbl_df")
  testthat::expect_setequal(names(test_financials), c("ticker", "type", "scraped_data", "clean_data"))
  testthat::expect_type(test_financials$scraped_data, "list")
  testthat::expect_type(test_financials$clean_data, "list")
  testthat::expect_setequal(unique(test_financials$type), c(type_fs, 'meta'))

  # test price

  test_file_price <- "expected_price_yahoo.rds"
  src_dir <- system.file("testdata", package = "companyDataScrapeR")
  src_price <- paste(src_dir, test_file_price, sep = "/")

  test_price <- get_yahoo_data(tickers, type = "price", start_date, end_date, frequency)
  expected_price <- readRDS(src_price)

  testthat::expect_equal(test_price$clean_data, expected_price$clean_data)

})
