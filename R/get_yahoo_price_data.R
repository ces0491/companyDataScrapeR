#' get price data from Yahoo
#'
#' @param pjs_session phantom.js session
#'
#' @return single column tbl of raw scraped text data
#'
get_yahoo_price_data <- function(pjs_session) {

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

#' clean price data table from Yahoo
#'
#' @param scraped_price_data tbl_df
#' @param frequency string
#'
#' @return tbl_df with date, variable and value
#'
clean_yahoo_price <- function(scraped_price_data, frequency) {

  assertR::assert_true(length(scraped_price_data) == 1, "logic error")

  if(grepl("Date", scraped_price_data[[1]][1])) {

    reqd_cols <- c("Open", "High", "Low", "Close*", "Adj Close**", "Volume")
    sprd_col_names <- paste(1:c(length(reqd_cols)))

    price_data <- scraped_price_data %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(date_str = substring(raw, 1, 12)) %>%
      dplyr::mutate(date_tmp = gsub(",", "", date_str)) %>%
      dplyr::mutate(date = as.Date(date_tmp, format = "%b %d %Y")) %>%
      dplyr::mutate(val_str = substring(raw, 14)) %>%
      dplyr::mutate(value = strsplit(val_str, " ")) %>%
      tidyr::unnest(value) %>%
      dplyr::mutate(value = gsub(",", "", value)) %>%
      dplyr::filter(stringr::str_detect(value, "\\d+")) %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      dplyr::group_by(raw) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(row, value) %>%
      dplyr::select(date, paste(1:c(length(reqd_cols)))) %>%
      tidyr::drop_na() %>%
      dplyr::rename_at(dplyr::vars(sprd_col_names), ~ reqd_cols)

    price_data_tbl <- price_data %>%
      tidyr::gather(variable, value, -date) %>%
      dplyr::mutate(value = ifelse(value >= 10, value / 100, value)) %>% # some values are quoted in cents while others in the currency unit
      dplyr::mutate(value = ifelse(value < 10, value * 100, value)) %>%
      dplyr::mutate(variable = stringr::str_remove_all(variable, "[^[:alnum:]]")) # remove special characters like *

    price_data_tbl <- dateR::to_period(price_data_tbl, frequency)

  } else {
    price_data_tbl <- tibble::tibble(date = as.Date(NA), variable = as.character(NA), value = as.numeric(NA))
  }

  price_data_tbl
}

