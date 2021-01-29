test_that("get_ticker_id returns the correct security name info", {

  ticker <- "GOOG"
  isin <- "ZAE000006896"
  name <- "Netcare"

  test_t <- get_ticker_id(ticker)
  test_i <- get_ticker_id(isin)
  test_n <- get_ticker_id(name)

  expected_t <- data.frame("ticker" = "GOOG", "name" = "Alphabet Inc Class C", "isin" = "US02079K1079")
  expected_i <- data.frame("ticker" = "SOLJ", "name" = "Sasol Ltd", "isin" = "ZAE000006896")
  expected_n <- data.frame("ticker" = "NTCJ", "name" = "Netcare", "isin" = "ZAE000011953")

  testthat::expect_equal(test_t, expected_t)
  testthat::expect_equal(test_i, expected_i)
  testthat::expect_equal(test_n, expected_n)

})
