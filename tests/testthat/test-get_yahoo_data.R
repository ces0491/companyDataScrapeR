test_that("get data from Yahoo Finance", {

  tickers <- c("TSLA", "CLS.JO")
  type_fs <- c("IS", "BS", "CFS")
  start_date <- "2020/07/01"
  end_date <- "2020/07/31"
  frequency <- "weekly"

  test_file_fin <- "expected_financials_yahoo.rds"
  test_file_price <- "expected_price_yahoo.rds"
  src_dir <- system.file("testdata", package = "companyDataScrapeR")

  src_fin <- paste(src_dir, test_file_fin, sep = "/")
  src_price <- paste(src_dir, test_file_price, sep = "/")

  test_financials <- get_yahoo_data(tickers, type_fs)
  expected_financials <- readRDS(src_fin)

  test_price <- get_yahoo_data(tickers, type = "price", start_date, end_date, frequency)
  expected_price <- readRDS(src_price)

  testthat::expect_equal(test_financials, expected_financials)
  testthat::expect_equal(test_price, expected_price)

})
