#' Get meta data for a ticker from Yahoo Finance
#'
#' @param pjs_session a phantom js session
#'
#' @return tbl_df with columns variable and value, containing the records Name, Market, Currency, Market Cap, Shares Outstanding and Beta
#'
pvt_get_yahoo_meta_data <- function(pjs_session) {

  # retrieve elements
  name_elem <- pjs_session$findElement(xpath = '//*[@id="quote-header-info"]/div[2]/div[1]/div[1]')
  name_raw <- name_elem$getText()

  gen_info_elem <- pjs_session$findElement(xpath = '//*[@id="quote-header-info"]/div[2]/div[1]/div[2]/span')
  gen_raw <- gen_info_elem$getText()

  stats_elem <- try(pjs_session$findElement(linkText = 'Statistics'), silent = TRUE)

  if (inherits(stats_elem, "try-error")) {
    mkt_cap_raw <- NA
    shares_raw <- NA
    beta_raw <- NA
  } else {
    stats_elem$click()
    mkt_cap_elem <- pjs_session$findElement(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[2]/div[1]/div/div/div/div/table/tbody/tr[1]')
    mkt_cap_raw <- mkt_cap_elem$getText()

    shares_elem <- pjs_session$findElement(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[2]/div[2]/div/div[2]/div/div/table/tbody/tr[3]')
    shares_raw <- shares_elem$getText()

    beta_elem <- pjs_session$findElement(xpath = '//*[@id="Col1-0-KeyStatistics-Proxy"]/section/div[2]/div[2]/div/div[1]/div/div/table/tbody/tr[1]')
    beta_raw <- beta_elem$getText()
  }

  fin_elem <- try(pjs_session$findElement(linkText = 'Financials'), silent = TRUE)
  if (inherits(stats_elem, "try-error")) {
    units_raw <- NA
    } else{
      fin_elem$click()
      units_elem <- pjs_session$findElement(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[2]/span/span')
      units_raw <- units_elem$getText()
  }

  # tidy
  ticker <- stringr::str_extract_all(name_raw,  "(?<=\\().+?(?=\\))")[[1]] # extract string in parentheses
  name <- stringr::str_remove(name_raw, ticker)
  name_clean <- stringr::str_sub(name, end = -4) # remove last 3 characters - parentheses and space
  name_df <- data.frame(variable = "Name", value = name_clean)

  market <- sub("\\-.*", "", gen_raw) # get string before hyphen
  market_df <- data.frame(variable = "Market", value = trimws(market))

  ccy_raw <- stringr::str_subset(gen_raw, "Currency in ")
  ccy <- stringr::str_sub(ccy_raw, start = -3)
  ccy_df <- data.frame(variable = "Currency", value = toupper(ccy))

  mkt_cap_val <- stringr::str_remove(mkt_cap_raw, "Market Cap \\(intraday\\) 5 ")
  mkt_cap_df <- data.frame(variable = "Market Cap", value = mkt_cap_val)

  shares_val <- stringr::str_remove(shares_raw, "Shares Outstanding 5 ")
  shares_df <- data.frame(variable = "Shares Outstanding", value = shares_val)

  beta_val <- stringr::str_remove(beta_raw, "Beta \\(5Y Monthly\\) ")
  beta_df <- data.frame(variable = "Beta", value = beta_val)

  units_df <- data.frame(variable = "Reporting Units", value = units_raw)

  # assemble

  meta_df <- name_df %>%
    dplyr::bind_rows(market_df) %>%
    dplyr::bind_rows(ccy_df) %>%
    dplyr::bind_rows(mkt_cap_df) %>%
    dplyr::bind_rows(shares_df) %>%
    dplyr::bind_rows(beta_df) %>%
    dplyr::bind_rows(units_df) %>%
    tibble::as_tibble(.)

  meta_df
}

#' Get meta data for a ticker from Yahoo Finance
#'
#' @param pjs_session phantom js session
#' @param url string indicating the url for a given tickers key statistics
#'
#' @return
#'
get_yahoo_meta_data <- function(pjs_session, url) {

  meta_url <- pjs_session$go(url)

  # yahoo will still navigate to a landing page even with a false ticker so we'll check first by looking for an expected element
  test_ticker_name <- try(pjs_session$findElement(xpath = '//*[@id="quote-header-info"]/div[2]/div[1]/div[1]'), silent = TRUE)

  if (inherits(test_ticker_name, "try-error")) {
    meta_df <- tibble::tibble(raw = NA)
  } else {
    meta_df <- pvt_get_yahoo_meta_data(pjs_session)
  }

  meta_df
}
