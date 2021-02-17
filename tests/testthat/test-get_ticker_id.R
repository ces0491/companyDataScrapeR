test_that("get_ticker_id returns the correct security name info", {

  names <- c("GOOG", "Netcare", "ZAE000006896")

  test <- get_ticker_id(names)

  expected <- tibble::tibble("ticker" = c("GOOG", "NTCJ", "SOLJ"),
                             "name" = c("Alphabet Inc Class C", "Netcare", "Sasol Ltd"),
                             "isin" = c("US02079K1079", "ZAE000011953", "ZAE000006896"))

  testthat::expect_equal(test, expected)

})
