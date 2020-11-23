test_that("get company data works as expected", {

  tickers <- c("YAH-TSLA", "INV-SOLJ")

  type <- c("price", "IS", "BS", "CFS")
  start_date <- "2020/07/01"
  end_date <- "2020/08/30"
  frequency <- "weekly"

  test_file <- "expected_company_data.rds"
  src_dir <- system.file("testdata", package = "companyDataScrapeR")

  src_file <- paste(src_dir, test_file, sep = "/")

  test <- get_company_data(tickers, type, start_date, end_date, frequency)

  expected <- readRDS(src_file)

  testthat::expect_equal(test, expected)

})
