
#' construct URLs required to scrape relevant data from Yahoo Finance
#'
#' @param tickers character vector of stock tickers following Yahoo Finance format i.e. JSE tickers have .JO suffix
#' @param type character vector indicating which types of data you require.
#' @param start_date start date for price data retrieval. Default NULL assumes no price data.
#' @param end_date end date for price data retrieval. Default NULL assumes no price data.
#'
#' @return tbl_df with cols ticker, page and url
#'
build_yahoo_url <- function(tickers, type = c("price", "IS", "BS", "CFS"), start_date = NULL, end_date = NULL) {

  ticker_list <- list()

  for (ticker in tickers) {

    is_url <- glue::glue("https://finance.yahoo.com/quote/{ticker}/financials?p={ticker}")
    bs_url <- glue::glue("https://finance.yahoo.com/quote/{ticker}/balance-sheet?p={ticker}")
    cf_url <- glue::glue("https://finance.yahoo.com/quote/{ticker}/cash-flow?p={ticker}")

    url_tbl <- tibble::tibble(IS = is_url,
                              BS = bs_url,
                              CFS = cf_url)

    if("price" %in% type) {
      assertR::assert_true(!is.null(start_date), "When requesting price data, you must specify a start date")
      assertR::assert_true(!is.null(end_date), "When requesting price data, you must specify an end date")
      assertR::assert_true(start_date < end_date, "start date is after end date")

      start_dt_epoch <- as.integer(as.POSIXct(start_date), tz = anytime:::getTZ())
      end_dt_epoch <- as.integer(as.POSIXct(end_date), tz = anytime:::getTZ())

      # we always assume a daily frequency in our query so that we can convert ourselves using dateR
      price_tbl <- tibble::tibble(price = glue::glue(
        "https://finance.yahoo.com/quote/{ticker}/history?period1={start_dt_epoch}&period2={end_dt_epoch}&interval=1d&filter=history&frequency=1d"))

      url_tbl <- price_tbl %>%
        dplyr::bind_cols(url_tbl)
    }

    ticker_list[[ticker]] <- url_tbl
  }

  req_type <- type

  ticker_tbl <- ticker_list %>%
    tibble::enframe(name = "ticker", value = "url") %>%
    tidyr::unnest(url) %>%
    tidyr::gather(type, url, -ticker) %>%
    dplyr::filter(type %in% req_type)

  ticker_tbl
}
