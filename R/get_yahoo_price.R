#' get price data from Yahoo
#'
#' @param pjs_session phantom.js session
#' @param url string specifying the url for the price data and its relevant parameters
#'
#' @return single column tbl of raw scraped text data
#'
get_yahoo_price_data <- function(pjs_session, url) {

  pjs_session <- pjs_session$go(url)

  # scroll to bottom of page
  for(i in 1:55) {
    webElem <- pjs_session$findElement(css = "body")
    webElem$sendKeys(webdriver::key$end)
    Sys.sleep(1)
  }

  price_tbl_elem <- try(webElem$findElement(xpath = '//*[@id="Col1-1-HistoricalDataTable-Proxy"]/section/div[2]/table'), silent = TRUE)

  if (inherits(price_tbl_elem, "try-error")) {
    price_tbl <- tibble::tibble(raw = NA)

  } else {

    price_tbl_raw <- price_tbl_elem$getText()
    price_tbl_txt <- unlist(strsplit(price_tbl_raw[[1]], "\n"))

    price_tbl <- tibble::tibble(raw = price_tbl_txt)

  }

  price_tbl
}



