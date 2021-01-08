#' scrape Yahoo Finance data
#'
#' @param pjs_session phantom.js session
#' @param ticker_tbl tbl_df with ticker, page and url
#' @param start_date start date for price data retrieval
#' @param end_date end date for price data retrieval
#'
#' @return object of class \code{list} named by each url specific to a ticker-type combination containing scraped data
#'
get_yahoo_data_list <- function(pjs_session, ticker_tbl, start_date, end_date) {

  assertR::assert_present(names(ticker_tbl), c("url", "type"))

  urls <- ticker_tbl$url
  yahoo_data_list <- list()

  for (url in urls) {

    u <- which(urls == url)
    progress <- round(u/length(urls), 2) * 100
    print(glue::glue("Attempting to retrieve {ticker_tbl$type[[u]]} data for {ticker_tbl$ticker[[u]]} from Yahoo Finance..."))

    if("price" %in% ticker_tbl$type[[u]]) {

      pjs_session$go(ticker_tbl$url[[u]])
      scraped_data <- get_yahoo_price_data(pjs_session)

    } else {

      pjs_session$go(ticker_tbl$url[[u]])
      scraped_data <- get_yahoo_fs_data(pjs_session)
    }

    print(glue::glue("{progress}% complete"))

    yahoo_data_list[[url]] <- scraped_data

  }

  yahoo_data_list

}

#' get tidy yahoo data
#'
#' @param tickers character vector of stock tickers following Yahoo Finance format i.e. JSE ticker EXAMPLE.JO
#' @param type character vector specifying the type of company data you want to retrieve
#' @param frequency string indicating the frequency of data to retrieve (if retrieving price data)
#' @param start_date start date for price data retrieval. Default NULL assumes no price data.
#' @param end_date end date for price data retrieval. Default NULL assumes no price data.
#'
#' @return object of class \code{tbl_df} with ticker, type, nested data.frame of scraped data and nested data.frame of tidy scraped data
#'
#' @importFrom magrittr %>%
#' @export
#'
get_yahoo_data <- function(tickers, type = c("price", "IS", "BS", "CFS"), start_date = NULL, end_date = NULL, frequency = NULL) {

  ticker_tbl <- build_yahoo_url(tickers, type, start_date, end_date)

  pjs_conn <- webScrapeR::connect_session(url = "https://finance.yahoo.com/")
  pjs_session <- pjs_conn$session

  yahoo_data_list <- get_yahoo_data_list(pjs_session, ticker_tbl, start_date, end_date)

  pjs_conn$pjs_process$kill()

  yahoo_data <- yahoo_data_list %>%
    tibble::enframe(name = "url", value = "scraped_data") %>%
    dplyr::left_join(ticker_tbl, by = "url") %>%
    dplyr::group_by(ticker, type) %>%
    dplyr::mutate(clean_data = purrr::map(scraped_data, clean_yahoo_data, type, frequency)) %>%
    dplyr::ungroup() %>%
    dplyr::select(ticker, type, scraped_data, clean_data)

  yahoo_data

}
