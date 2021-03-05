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

  price_data_tbl <- price_df_long %>%
    dplyr::group_by(variable) %>%
    dateR::to_period(., frequency) %>%
    dplyr::ungroup() %>%
    data.frame(.)

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

  rep_units <- scraped_fs_data[1, ] # the reporting units are retrieved at the get_fs stage for reference but are not used in the output here

  dt_raw <- scraped_fs_data[2:9, ]
  dt_txt <- unlist(stringr::str_extract_all(dt_raw, '[0-9]+'))

  dts_list <- list()
  for(n in 1:12){
    if(n %% 3 == 1) {
      dt_sub <- dt_txt[n:(n+2)]
      dt <- paste(dt_sub, collapse = "/")
    }
    dts_list[[n]] <- dt
  }

  dts <- dts_list %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = value) %>%
    dplyr::distinct(value) %>%
    dplyr::pull(value)

  n_data_cols <- length(dts)

  fs_df <- scraped_fs_data %>%
    dplyr::filter(dplyr::row_number() > 9) %>%
    dplyr::mutate(variable = gsub('[0-9]+', "", raw_text)) %>%
    dplyr::mutate(variable = gsub('\\.', "", variable)) %>%
    dplyr::mutate(variable = gsub('-', "", variable)) %>%
    dplyr::mutate(variable = trimws(variable, "both")) %>%
    dplyr::mutate(value = gsub("[aA-zZ]", "", raw_text)) %>%
    dplyr::mutate(value = trimws(value, "both")) %>%
    dplyr::mutate(value = stringr::str_split(value, " ")) %>%
    dplyr::mutate(value = gsub("[^[:alnum:][:blank:]+\\.-]", "", value)) %>%
    dplyr::mutate(value = gsub("c", "", value)) %>%
    dplyr::mutate(value = gsub(".  ", "", value)) %>%
    dplyr::mutate(value = trimws(value, "both")) %>%
    dplyr::mutate(value = gsub('^- ',"",value)) %>%
    tidyr::separate(value, dts, " ", remove = FALSE) %>%
    dplyr::select(-raw_text, -value)

  assertR::assert_true(length(fs_df) == n_data_cols + 1, "check number of columns being produced in fs_df vs available columns from investing.com")

  fs_df_long <- fs_df %>%
    tidyr::gather(date, value, -variable) %>%
    dplyr::mutate(date = as.Date(date, format = "%Y/%d/%m")) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%  # there will be dashes from the raw data to denote missing values. these should be NA so ignore warning
    dplyr::select(date, variable, value)

  fs_df_long

}

#' clean Investing.com data
#'
#' @param scraped_data tbl_df of scraped data
#' @param type string indicating the data type, e.g. 'IS'
#' @param frequency string
#'
#' @return tbl_df
#'
clean_invcom_data <- function(scraped_data, type, frequency = NULL) {

  assertR::assert_true(length(type) == 1, "logic error - only one type at a time")

  if("price" %in% type) {
    assertR::assert_present(c("daily", "weekly", "monthly", "quarterly", "annual"), frequency)
  }

  clean_df <- switch(type,
                     price = clean_invcom_price(scraped_data, frequency),
                     IS = clean_invcom_fs(scraped_data, type),
                     BS = clean_invcom_fs(scraped_data, type),
                     CFS = clean_invcom_fs(scraped_data, type))

  clean_df
}
