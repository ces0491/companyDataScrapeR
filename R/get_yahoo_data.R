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

  yahoo_data_list <- list()

  for (i in 1:nrow(ticker_tbl)) {

    url <- ticker_tbl$url[[i]]
    ticker <- ticker_tbl$ticker[[i]]
    type <- ticker_tbl$type[[i]]

    print(glue::glue("Attempting to retrieve {type} data for {ticker} from Yahoo Finance..."))

    scraped_data <- switch(type,
                           price = get_yahoo_price_data(pjs_session, url),
                           IS = get_yahoo_fs_data(pjs_session, url),
                           BS = get_yahoo_fs_data(pjs_session, url),
                           CFS = get_yahoo_fs_data(pjs_session, url),
                           meta = get_yahoo_meta_data(pjs_session, url))

    progress <- round(i/nrow(ticker_tbl), 2) * 100
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
    dplyr::group_by(url) %>%
    dplyr::mutate(clean_data = purrr::map(scraped_data, clean_yahoo_data, type, frequency)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(clean_data = ifelse(type == "meta", scraped_data, clean_data)) %>%
    dplyr::select(ticker, type, scraped_data, clean_data)

  yahoo_data

}
