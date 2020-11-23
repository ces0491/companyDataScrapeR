#' clean price data table from Investing.com
#'
#' @param scraped_price_data tbl_df of scraped data
#' @param frequency string indicating the frequency to return price data, e.g. 'monthly'
#'
#' @return object of class \code{tbl_df} with date, variable and value
#'
clean_invcom_price <- function(scraped_price_data, frequency) {

  assertR::assert_true(class(scraped_price_data) == "data.frame", "logic error - scraped price data needs to be a data.frame")
  assertR::assert_true(length(scraped_price_data) == 1, "logic error - only one data.frame at a time")
  assertR::assert_present(c("daily", "weekly", "monthly", "quarterly", "annual"), frequency)

  reqd_cols <- c('Price', 'Open', 'High', 'Low')
  headers <- strsplit(scraped_price_data[1,], "\\s")[[1]][1:7]

  price_df <- scraped_price_data %>%
    dplyr::rename("all_string" = 1) %>%
    dplyr::filter(dplyr::row_number() != 1) %>%
    dplyr::mutate(date_str = substr(all_string, 1, 13)) %>%
    dplyr::mutate(price_str = stringr::str_replace_all(all_string, date_str, "")) %>%
    tidyr::separate(price_str, into = headers[2:7], sep = "\\s") %>%
    dplyr::select(date_str, dplyr::all_of(reqd_cols)) %>%
    dplyr::mutate(day = substr(date_str, 5, 6)) %>%
    dplyr::mutate(month = substr(date_str, 1, 3)) %>%
    dplyr::mutate(year = substr(date_str, 9, 12)) %>%
    tidyr::unite(date, year, month, day, sep = "/") %>%
    dplyr::mutate(date = as.Date(date, "%Y/%b/%d")) %>%
    dplyr::select(date, dplyr::all_of(reqd_cols))

  price_df_long <- price_df %>%
    tidyr::gather(variable, value, -date) %>%
    dplyr::group_by(variable) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(value = gsub(",", "", value)) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) # strings will be used to denote missing values so they will be converted to NA. we know this so ignore

  price_data_tbl <- dateR::to_period(price_df_long, frequency)

  price_data_tbl
}

#' clean financial statement tables from Yahoo
#'
#' @param scraped_fs_data tbl_df of scraped financial statement data
#' @param type string indicating the data type, e.g. 'IS'
#'
#' @return object of class \code{tbl_df} with date, variable and value
#'
clean_invcom_fs <- function(scraped_fs_data, type) {

  dt_header <- scraped_fs_data[1:8, ][c(1,3,5,7)]
  dt_header <- stringr::str_extract(dt_header, '[0-9]+')
  n_data_cols <- length(dt_header)

  fs_df <- scraped_fs_data %>%
    dplyr::rename("all_string" = 1) %>%
    dplyr::filter(dplyr::row_number() > 8) %>%
    dplyr::mutate(variable = gsub('[0-9]+', "", all_string)) %>%
    dplyr::mutate(variable = gsub('\\.', "", variable)) %>%
    dplyr::mutate(variable = gsub('-', "", variable)) %>%
    dplyr::mutate(variable = trimws(variable, "both")) %>%
    dplyr::mutate(value = gsub("[aA-zZ]", "", all_string)) %>%
    dplyr::mutate(value = trimws(value, "both")) %>%
    dplyr::mutate(value = stringr::str_split(value, " ")) %>%
    dplyr::mutate(value = gsub("[^[:alnum:][:blank:]+\\.-]", "", value)) %>%
    dplyr::mutate(value = gsub("c", "", value)) %>%
    dplyr::mutate(value = gsub(".  ", "", value)) %>%
    dplyr::mutate(value = trimws(value, "both")) %>%
    dplyr::mutate(value = gsub('^- ',"",value)) %>%
    tidyr::separate(value, dt_header, " ", remove = FALSE) %>%
    dplyr::select(-all_string, -value)

  assertR::assert_true(length(fs_df) == n_data_cols + 1, "check number of columns being produced in fs_df vs available columns from investing.com")

  fs_df_long <- fs_df %>%
    tidyr::gather(year, value, -variable) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) # there will be dashes from the raw data to denote missing values. these should be NA so ignore warning

  fs_df_long

}

#' clean Investing.com data
#'
#' @param scraped_data tbl_df of scraped data
#' @param type string indicating the data type, e.g. 'IS'
#' @param ... arguments to other methods
#'
#' @return tbl_df
#'
clean_invcom_data <- function(scraped_data, type, ...) {

  assertR::assert_true(length(type) == 1, "logic error - only one type at a time")

  if("price" %in% type) {
    clean_df <- clean_invcom_price(scraped_data, frequency)
  } else {
    clean_df <- clean_invcom_fs(scraped_data, type)
  }

  clean_df
}
