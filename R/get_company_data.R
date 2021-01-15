#' Get company data from Yahoo Finance or Investing.com
#'
#' @param tickers character vector of tickers where data is prefixed with the source, e.g. YAH-EXAMPLETICKER
#' @param type string specifying the type of data required - one of is, bs, cfs or price
#' @param start_date start date
#' @param end_date end date
#' @param frequency string indicating the frequency to return price data, e.g. 'monthly'
#'
#' @return object of class \code{tbl_df} with columns, ticker, type and nested raw and clean company data
#'
#' @importFrom magrittr %>% %$%
#' @export
#'
get_company_data <- function(tickers, type, start_date, end_date, frequency) {

  assertR::assert_true(all(grepl("-", tickers)), "Check your tickers - Remember to specify a prefix to indicate the data source")
  assertR::assert_present(c("price", "IS", "BS", "CFS"), type)

  # split ticker vector between source prefix and actual ticker
  unq_tickers <- unique(tickers)
  ticker_df <- data.frame(ticker = unq_tickers)
  ticker_tbl <- ticker_df %>%
    tidyr::separate(ticker, c("d_source", "ticker"), sep = "-", remove = TRUE)

  yah_tkrs <- ticker_tbl %>%
    dplyr::filter(d_source == "YAH") %$%
    ticker

  inv_tkrs <- ticker_tbl %>%
    dplyr::filter(d_source == "INV") %$%
    ticker

  all_co_data <- tibble::tibble(ticker = character(), type = character(), scraped_data = list(), clean_data = list())

  # retrieve data from yahoo finance and/or investing.com
  if (length(yah_tkrs) > 0) {
    message("Attempting to retrieve data from Yahoo Finance...")
    yahoo_data <- get_yahoo_data(yah_tkrs, type, start_date, end_date, frequency)
    assertR::assert_present(names(yahoo_data), c("ticker", "type", "scraped_data", "clean_data"))
  } else {
    yahoo_data <- NULL
  }

  if (length(inv_tkrs) > 0) {
    message("Attempting to retrieve data from Investing.com...")
    invcom_data <- get_invcom_data(inv_tkrs, type, start_date, end_date, frequency)
    assertR::assert_present(names(invcom_data), c("ticker", "type", "scraped_data", "clean_data"))
  } else {
    invcom_data <- NULL
  }

  # combine data
  all_co_data <- all_co_data %>%
    dplyr::bind_rows(yahoo_data) %>%
    dplyr::bind_rows(invcom_data)

  dupes_tmp <- assertR::assert_duplicates(tickers)

  dupes <- tickers[which(duplicated(tickers))]
  dupes_str <- paste(dupes, collapse = "','")
  dupes_str <- paste("'", dupes_str, "'", sep = "")

  if (length(dupes) > 0) {
    warning(glue::glue("The following duplicate values were found in argument 'tickers' and were removed: {dupes_str}"))
  }

  all_co_data
}
