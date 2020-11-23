#' Navigate to the ticker home page from the investing.com home page
#'
#' @param pjs_session phantom.js session
#' @param home_url investing.com home url
#' @param ticker string indicating the ticker
#'
navigate_ticker_home <- function(pjs_session, home_url, ticker) {

  pjs_session$go(home_url)

  search_elem <- pjs_session$findElement(xpath = '/html/body/div[5]/header/div[1]/div/div[3]/div[1]/input')
  search_elem$click()
  search_elem$sendKeys(ticker, webdriver::key$enter)

  Sys.sleep(1)

  ticker_elem <- pjs_session$findElement(xpath = '//*[@id="fullColumn"]/div/div[2]/div[2]/div[1]/a')
  ticker_elem$click()

  pjs_session
}


#' scrape Investing.com Finance data
#'
#' @param pjs_session phantom.js session
#' @param ticker_tbl tbl_df with ticker, page and url
#' @param start_date start date for price data retrieval
#' @param end_date end date for price data retrieval
#'
#' @return object of class \code{list} named by each url specific to a ticker-type combination containing scraped data
#'
get_invcom_data_list <- function(pjs_session, ticker_tbl, start_date, end_date) {

  invcom_home <- "https://uk.investing.com/"

  assertR::assert_present(names(ticker_tbl), c("ticker", "type"))

  tickers <- ticker_tbl$ticker
  types <- ticker_tbl$type
  assertR::assert_true(length(tickers) == length(types))

  invcom_data_list <- list()

    for (n in 1:nrow(ticker_tbl)) {

    unq <- paste(ticker_tbl$ticker[[n]], ticker_tbl$type[[n]], sep = '_')

    pjs_session <- navigate_ticker_home(pjs_session, invcom_home, tickers[[n]]) # shouldn't need to do this each time if ticker is the same

    progress <- round(n/nrow(ticker_tbl), 2) * 100
    print(glue::glue("Attempting to retrieve {types[[n]]} data for {tickers[[n]]} from Investing.com..."))

    if("price" %in% types[[n]]) {

      scraped_data <- get_invcom_price_data(pjs_session, start_date, end_date)

    } else {

      scraped_data <- get_invcom_fs_data(pjs_session, type = types[[n]])
    }

    print(glue::glue("{progress}% complete"))

    invcom_data_list[[unq]] <- scraped_data

  }

  invcom_data_list

}

#' Get Investing.com data
#'
#' @param tickers character vector
#' @param type character vector
#' @param start_date date
#' @param end_date date
#' @param frequency string
#'
#' @return tbl_df
#'
#' @importFrom magrittr %>%
#' @export
#'
get_invcom_data <- function(tickers, type = c("price", "IS", "BS", "CFS"), start_date = NULL, end_date = NULL, frequency = NULL) {

  pjs_conn <- webScrapeR::connect_session("https://uk.investing.com/")
  pjs_session <- pjs_conn$session

  ticker_tbl <- data.frame(ticker = tickers) %>%
    tidyr::expand(ticker, type)

  invcom_data_list <- get_invcom_data_list(pjs_session, ticker_tbl, start_date, end_date)

  pjs_conn$pjs_process$kill()

  invcom_data <- invcom_data_list %>%
    tibble::enframe(name = "id", value = "scraped_data") %>%
    tidyr::separate(id, c("ticker", "type")) %>%
    dplyr::group_by(ticker, type) %>%
    dplyr::mutate(clean_data = purrr::map(scraped_data, clean_invcom_data, type, frequency)) %>%
    dplyr::ungroup() %>%
    dplyr::select(ticker, type, scraped_data, clean_data)

  invcom_data
}
