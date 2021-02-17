#' Navigate to the ticker home page
#'
#' @param pjs_session phantom.js session
#' @param ticker string indicating the ticker
#'
navigate_ticker_home <- function(pjs_session, ticker) {

  search_elem <- pjs_session$findElement(xpath = '/html/body/div[5]/header/div[2]/div/div[3]/div[1]/input')
  search_elem$click()
  search_elem$sendKeys(ticker, webdriver::key$enter)

  Sys.sleep(1)

  pjs_session <- tryCatch(
    {
      ticker_elem <- pjs_session$findElement(xpath = '//*[@id="fullColumn"]/div/div[2]/div[2]/div[1]/a')
      ticker_elem$click()
    },
    error = function(e) {
      message(glue::glue("Could not find {ticker} on Investing.com"))
      return(NULL)
    }
  )

  pjs_session
}

#' private function to navigate home and return the ticker's meta data
#'
#' @param pjs_session phantom.js session
#' @param ticker string
#'
#' @return data.frame of meta data associated with the ticker
#'
pvt_get_meta <- function(pjs_session, ticker) {
  print(glue::glue("Navigating to {ticker} home..."))
  pjs_session <- navigate_ticker_home(pjs_session, ticker)
  print(glue::glue("Attempting to retrieve meta data for {ticker} from Investing.com..."))
  meta <- get_invcom_meta_data(pjs_session)
  meta
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

  assertR::assert_present(names(ticker_tbl), c("id", "ticker", "type"))

  invcom_data_list <- list()

    for (i in 1:nrow(ticker_tbl)) {

    id <- ticker_tbl$id[[i]]
    ticker <- ticker_tbl$ticker[[i]]
    type <- ticker_tbl$type[[i]]

    if(i == 1) {
      meta  <- pvt_get_meta(pjs_session, ticker)
    } else {
      if(i > 1 & ticker != ticker_tbl$ticker[[i - 1]]) {
        meta  <- pvt_get_meta(pjs_session, ticker)
      }
    }

    invcom_data_list[[paste0(ticker, "_meta")]] <- meta

    print(glue::glue("Attempting to retrieve {type} data for {ticker} from Investing.com..."))

    scraped_data <- switch(type,
                           price = get_invcom_price_data(pjs_session, start_date, end_date),
                           IS = get_invcom_fs_data(pjs_session, type),
                           BS = get_invcom_fs_data(pjs_session, type),
                           CFS = get_invcom_fs_data(pjs_session, type))

    progress <- round(i/nrow(ticker_tbl), 2) * 100
    print(glue::glue("{progress}% complete"))

    invcom_data_list[[id]] <- scraped_data

  }

  invcom_data_list

}

#' Get Investing.com data
#'
#' @param tickers character vector of investing.com tickers
#' @param type character vector specifying the type of data required - price, income statement, balance sheet and/or cash flow statement
#' @param start_date date
#' @param end_date date
#' @param frequency string indicating the frequency of data to return - required for price data
#'
#' @return tbl_df with cols ticker, type, scraped_data and clean_data
#'
#' @importFrom magrittr %>%
#' @export
#'
get_invcom_data <- function(tickers, type = c("price", "IS", "BS", "CFS"), start_date = NULL, end_date = NULL, frequency = NULL) {

  pjs_conn <- webScrapeR::connect_session("https://uk.investing.com/")
  pjs_session <- pjs_conn$session

  ticker_tbl <- data.frame(ticker = tickers) %>%
    tidyr::expand(ticker, type = c(type)) %>%
    dplyr::group_by(ticker) %>%
    dplyr::arrange(desc(type), .by_group = TRUE) %>% # hack to make price first - think of better way to enforce order or make order irrelevant
    dplyr::ungroup() %>%
    tidyr::unite("id", c("ticker", "type"), sep = "_", remove = FALSE)

  invcom_data_list <- get_invcom_data_list(pjs_session, ticker_tbl, start_date, end_date)

  pjs_conn$pjs_process$kill()

  invcom_data <- invcom_data_list %>%
    tibble::enframe(name = "id", value = "scraped_data") %>%
    tidyr::separate(id, c("ticker", "type")) %>%
    dplyr::group_by(ticker, type) %>%
    dplyr::mutate(clean_data = purrr::map(scraped_data, clean_invcom_data, type, frequency)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(clean_data = ifelse(type == "meta", scraped_data, clean_data)) %>%
    dplyr::select(ticker, type, scraped_data, clean_data)

  invcom_data

}
