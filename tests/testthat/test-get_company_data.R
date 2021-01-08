test_that("get company data works as expected", {

  tickers <- c("YAH-TSLA", "INV-SOLJ")

  type <- c("price", "IS", "BS", "CFS")
  start_date <- "2020/07/01"
  end_date <- "2020/08/30"
  frequency <- "weekly"

  test <- get_company_data(tickers, type, start_date, end_date, frequency)

  # because this data is only available on a rolling window, it will change month to month and year to year.
  # we therefore can't reliably test the output so we check the returned data structure instead.
  testthat::expect_is(test, "tbl_df")
  testthat::expect_true(ncol(test) == 4)
  testthat::expect_true(names(test) == c("ticker", "type", "scraped_data", "clean_data"))
  testthat::expect_is(test$scraped_data, "list")
  testthat::expect_is(test$clean_data, "list")
  testthat::expect_true(nrow(test) == 8)

})
