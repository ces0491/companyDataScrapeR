#' Get company data from Yahoo Finance or Investing.com
#'
#' @param tickers character vector of tickers where data is prefixed with the source, e.g. YAH-EXAMPLETICKER
#' @param type string specifying the type of data required - one of is, bs, cfs or price
#' @param start_date start date
#' @param end_date end date
#' @param frequency string indicating the frequency to return price data, e.g. 'monthly'
#'
#' @return a data.frame
#'
get_company_data_single <- function(tickers, type, start_date, end_date, frequency) {

  assertR::assert_present(type, c("price", "IS", "BS", "CFS"), "You've specified an unsupported type")

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

  # retrieve data from yahoo finance and/or investing.com
  if (length(yah_tkrs) > 0) {
    message("Attempting to retrieve data from Yahoo Finance...")
    yahoo_data <- get_yahoo_data(yah_tkrs, type, start_date, end_date, frequency)
    assertR::assert_present(names(yahoo_data), c("date", "ticker"))
  } else {
    yahoo_data <- NULL
  }

  if (length(inv_tkrs) > 0) {
    message("Attempting to retrieve data from Investing.com...")
    invcom_data <- get_invcom_data(inv_tkrs, type, start_date, end_date, frequency)
    assertR::assert_present(names(invcom_data), c("date", "ticker"))
  } else {
    invcom_data <- NULL
  }

  # combine data
  all_co_data <- yahoo_data %>%
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

#' Get financial statement data from Yahoo Finance or Investing.com
#'
#' @param tickers character vector of tickers with the relevant prefix for the data source, e.g. 'YAH-'
#' @param type character vector indicating which data type to retrieve
#' @param start_date start date
#' @param end_date end date
#' @param frequency string indicating the frequency to return price data, e.g. 'monthly'
#'
#' @return object of class \code{tbl_df} with columns, date, ticker, type and nested company data
#'
#' @importFrom magrittr %>%
#' @export
#'
get_company_data <- function(tickers, type = c("price", "IS", "BS", "CFS"), start_date = NULL, end_date = NULL, frequency = NULL) {

  assertR::assert_true(all(grepl("-", tickers)), "Check your tickers - Remember to specify a prefix to indicate the data source")

  all_fs_data <- tibble::tibble(date = as.Date(character()), ticker = character(), statement = character(), fs_data = list())

  for(t in type) {
    print(glue::glue("Retrieving {t} data..."))
    single_co_data <- get_company_data_single(tickers, type = t, start_date, end_date, frequency)
    all_co_data <- dplyr::bind_rows(all_co_data, single_co_data)
  }

  all_co_data
}
