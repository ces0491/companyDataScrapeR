#' Get Ticker ID
#' Use investing.com to retrieve name info for a given security
#'
#' @param ticker_id string containing the name, ticker or ISIN code of the security of interest.
#' Best practice is to use the ISIN as that will always return a unique result for the expected security.
#' Using tickers or names may result in multiple matches in which case the first match is returned.
#'
#' @return data.frame with columns ticker, name and isin
#' @export
#'
get_ticker_id <- function(ticker_id) {

  url <- "https://uk.investing.com/"
  pjs_conn <- webScrapeR::connect_session(url)
  pjs_session <- pjs_conn$session
  pjs_session <- navigate_ticker_home(pjs_session, url, ticker_id)

  assertR::assert_true(!is.null(pjs_session), "No phantom.js session detected")

  name_elem <- pjs_session$findElement(xpath = '//*[@id="leftColumn"]/div[1]')
  ticker_name <- name_elem$getText()

  gen_elem <- pjs_session$findElement(xpath = '//*[@id="quotes_summary_current_data"]/div[2]')
  general_info <- gen_elem$getText()

  pjs_conn$pjs_process$kill()

  # clean

  name_split <- strsplit(ticker_name, "\n", fixed = TRUE)[[1]]
  name_raw <- name_split[1]
  ticker <- stringr::str_extract_all(name_raw,  "(?<=\\().+?(?=\\))")[[1]] # extract string in parentheses
  name <- stringr::str_remove(name_raw, ticker)
  name <- stringr::str_sub(name, end = -4) # remove last 3 characters - parentheses and space

  info_split <- strsplit(general_info, "\n", fixed = TRUE)[[1]]
  isin_raw <- stringr::str_subset(info_split, "ISIN")
  isin <- stringr::str_remove(isin_raw, "ISIN:")
  isin <- trimws(isin, "both")

  result <- data.frame("ticker" = ticker, "name" = name, "isin" = isin)
  result
}
