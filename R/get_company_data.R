#' Get company data from Yahoo Finance or Investing.com
#'
#' @param tickers character vector of tickers where data is prefixed with the source, e.g. YAH-EXAMPLETICKER
#' @param type string specifying the type of data required - one of is, bs, cfs or price
#'
#' @return a data.frame
#'
get_company_data_single <- function(tickers, type = c("is", "bs", "cfs", "price")) {

  assertR::assert_present(c("is", "bs", "cfs", "price"), type, "check that you specified the financial statement type correctly")

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
    message("retrieving data from Yahoo Finance...")
    yahoo_data <- get_yahoo_data()
    assertR::assert_present(names(yahoo_data), c("date", "ticker"))
  } else {
    yahoo_data <- NULL
  }

  # retrieve data from yahoo finance and/or investing.com
  if (length(inv_tkrs) > 0) {
    message("retrieving data from Investing.com...")
    invcom_data <- get_invcom_data()
    assertR::assert_present(names(invcom_data), c("date", "ticker"))
  } else {
    invcom_data <- NULL
  }

  # combine data
  all_co_data <- yahoo_data %>%
    dplyr::bind_rows(invcom_data)

  dupes <- tickers[which(duplicated(tickers))]
  dupes_str <- paste(dupes, collapse = "','")
  dupes_str <- paste("'", dupes_str, "'", sep = "")

  if (length(dupes) > 0) {
    warning(glue::glue("The following duplicate values were found in argument 'tickers' and were removed: {dupes_str}"))
  }

  all_co_data
}

#' Get financial statement data from Yahoo, Investing.com or the SEC
#'
#' @param tickers character vector of tickers - preferably isins for morningstar data, with the relevent prefix for SEC or MS data
#' @param type character vector indicating which financial statements to retrieve
#'
#' @return tibble with cols, date, ticker, type and nested company data
#' @export
#'
get_company_data <- function(tickers, type = c("is", "bs", "cfs", "price")) {

  all_fs_data <- tibble::tibble(date = as.Date(character()), ticker = character(), statement = character(), fs_data = list())

  for(t in type) {
    print(glue::glue("retrieving {t} data..."))
    single_co_data <- get_company_data_single(tickers, type = t)
    all_co_data <- dplyr::bind_rows(all_co_data, single_co_data)
  }

  all_co_data
}
