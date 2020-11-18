test_that("get data from Investing.com", {

  tickers <- c("GOOG", "AFTJ")
  type_fs <- c("IS", "BS", "CFS")
  start_date <- "2020/07/31"
  end_date <- "2020/10/31"
  frequency <- "monthly"

  test_file_fin <- "expected_financials_invcom.rds"
  test_file_price <- "expected_price_invcom.rds"
  src_dir <- system.file("testdata", package = "companyDataScrapeR")

  src_fin <- paste(src_dir, test_file_fin, sep = "/")
  src_price <- paste(src_dir, test_file_price, sep = "/")

  # test get financial data
  test_financials <- get_invcom_data(tickers, type_fs)

  test_clean_fin <- test_financials %>%
    dplyr::select(-scraped_data) %>%
    tidyr::unnest(clean_data) %>%
    dplyr::filter(date == sort(unique(date), decreasing = TRUE)[2]) # get the 2nd most recent numbers. the most recent will be TTM which will change all the time. The others will roll out per year.

  expected_financials <- readRDS(src_fin)

  expected_clean_fin <- expected_financials %>%
    dplyr::select(-scraped_data) %>%
    tidyr::unnest(clean_data) %>%
    dplyr::filter(date == sort(unique(date), decreasing = TRUE)[2])

  # test get price data
  test_price <- get_invcom_data(tickers, type = "price", start_date, end_date, frequency)
  expected_price <- readRDS(src_price)

  testthat::expect_equal(test_clean_fin, expected_clean_fin)
  testthat::expect_equal(test_price$clean_data, expected_price$clean_data)

})
