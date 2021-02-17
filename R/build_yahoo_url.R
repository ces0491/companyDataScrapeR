
#' construct URLs required to scrape relevant data from Yahoo Finance
#'
#' @param tickers character vector of stock tickers following Yahoo Finance format i.e. JSE tickers have .JO suffix
#' @param type character vector indicating which types of data you require.
#' @param start_date start date for price data retrieval. Default NULL assumes no price data.
#' @param end_date end date for price data retrieval. Default NULL assumes current date.
#'
#' @return tbl_df with cols ticker, page and url
#'
build_yahoo_url <- function(tickers, type = c("price", "IS", "BS", "CFS"), start_date = NULL, end_date = NULL) {

  ticker_list <- list()

  for (ticker in tickers) {

    is_url <- glue::glue("https://finance.yahoo.com/quote/{ticker}/financials?p={ticker}")
    bs_url <- glue::glue("https://finance.yahoo.com/quote/{ticker}/balance-sheet?p={ticker}")
    cf_url <- glue::glue("https://finance.yahoo.com/quote/{ticker}/cash-flow?p={ticker}")
    meta_url <- glue::glue("https://finance.yahoo.com/quote/{ticker}?p={ticker}&.tsrc=fin-srch")

    url_tbl <- tibble::tibble(IS = is_url,
                              BS = bs_url,
                              CFS = cf_url,
                              meta = meta_url)

    if("price" %in% type) {
      assertR::assert_true(!is.null(start_date), "When requesting price data, you must specify a start date")
      assertR::assert_true(start_date < end_date, "start date is after end date")

      start_dt_epoch <- as.integer(as.POSIXct(start_date), tz = anytime:::getTZ())

      if(is.null(end_date)) end_date <- Sys.Date()
      end_dt_epoch <- as.integer(as.POSIXct(end_date), tz = anytime:::getTZ())

      # we always assume a daily frequency in our query so that we can convert ourselves using dateR
      # "https://finance.yahoo.com/quote/{ticker}/history?period1={start_dt_epoch}&period2={end_dt_epoch}&interval=1d&filter=history&frequency=1d"
      price_tbl <- tibble::tibble(price = glue::glue(
        "https://query1.finance.yahoo.com/v7/finance/download/{ticker}?period1={start_dt_epoch}&period2={end_dt_epoch}&interval=1d&events=history&includeAdjustedClose=true.csv"
        ))

      url_tbl <- price_tbl %>%
        dplyr::bind_cols(url_tbl)
    }

    ticker_list[[ticker]] <- url_tbl
  }

  reqd_fs <- toupper(setdiff(type, c('price', 'meta')))

  if('price' %in% type) {
    reqd_type <- c(reqd_fs, "price", "meta")
  } else {
    reqd_type <- c(reqd_fs, "meta")
  }

  ticker_tbl <- ticker_list %>%
    tibble::enframe(name = "ticker", value = "url") %>%
    tidyr::unnest(url) %>%
    tidyr::gather(type, url, -ticker) %>%
    dplyr::filter(type %in% reqd_type)

  ticker_tbl
}
