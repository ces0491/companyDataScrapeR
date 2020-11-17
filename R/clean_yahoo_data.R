#' clean price data table from Yahoo
#'
#' @param scraped_price_data tbl_df of scraped data
#' @param frequency string indicating the frequency to return price data, e.g. 'monthly'
#'
#' @return object of class \code{tbl_df} with date, variable and value
#'
clean_yahoo_price <- function(scraped_price_data, frequency) {

  assertR::assert_present(c("daily", "weekly", "monthly", "quarterly", "annual"), frequency, "check that you specified the frequency correctly")
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
      dplyr::rename_at(dplyr::all_of(sprd_col_names), ~ reqd_cols)

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

#' clean financial statement tables from Yahoo
#'
#' @param scraped_fs_data tbl_df of scraped financial statement data
#' @param type string indicating the data type, e.g. 'IS'
#'
#' @return object of class \code{tbl_df} with date, variable and value
#'
clean_yahoo_fs <- function(scraped_fs_data, type) {

  if (all(is.na(scraped_fs_data$raw)) | length(scraped_fs_data$raw) < 4) {

    fs_data_tbl <- tibble::tibble(date = as.Date(NA), variable = as.character(NA), value = as.numeric(NA))

  } else {

    dt_tbl <-  scraped_fs_data %>%
      dplyr::filter(grepl("\\/", raw)) %>%
      dplyr::mutate(split_1 = strsplit(raw, "\\/")) %>%
      tidyr::unnest(split_1) %>%
      dplyr::mutate(year = substring(split_1, 1,4)) %>%
      dplyr::mutate(year = ifelse(grepl("TTM", split_1), format(Sys.Date(), "%Y"), year)) %>%
      dplyr::filter(nchar(year) == 4) %>%
      dplyr::mutate(raw2 = gsub("TTM", "", raw)) %>%
      dplyr::mutate(month = sub("\\/.*", "", raw2)) %>%
      dplyr::mutate(month = ifelse(grepl("TTM", split_1), as.numeric(format(Sys.Date(), "%m"))-1, month)) %>%
      dplyr::mutate(day = 1) %>%
      tidyr::unite(date_str, year,month,day, sep = "-") %>%
      dplyr::mutate(date = dateR::get_eom_dates(as.Date(date_str))) %>%
      dplyr::mutate(col = dplyr::row_number()) %>%
      dplyr::select(raw, raw2, date, col)

    cnt_dt <- length(dt_tbl$date)

    # scraped data will always begin with 'Breakdown' (the variable column header) and the dates
    reqd_raw <- scraped_fs_data %>%
      dplyr::filter(raw != "Breakdown") %>%
      dplyr::filter(raw != dt_tbl$raw[1])

    val_str <- reqd_raw %>%
      dplyr::mutate(value = stringr::str_split(raw, "(?<!-)\\s")) %>% # split string either on space or a hyphen followed by a space (negative numbers)
      tidyr::unnest(value) %>%
      dplyr::mutate(value = gsub("\\s", "", value)) %>% # remove any spaces that may be left as a result of negative numbers
      dplyr::mutate(value = gsub(",", "", value)) %>%
      dplyr::filter(stringr::str_detect(value, "\\d+")) %>% # hyphens may be left behind. just keep the numbers before converting
      dplyr::mutate(dbl_hyph = substr(value, 1, 2)) %>% # sometimes a blank cell (denoted with a hyphen) exists before a negative value and so a double hyphen prefix occurs
      dplyr::mutate(value = ifelse(dbl_hyph == "--", substring(value, 2), value)) %>%
      dplyr::select(-dbl_hyph)

    val_tbl <- val_str %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      dplyr::select(raw, value) %>%
      dplyr::group_by(raw) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(row, value) %>% # annoyingly, the values get reordered after the spread so rejoin reqd raw by raw to get the order back
      dplyr::select(1:(cnt_dt + 1))

    fs_data <- dplyr::left_join(reqd_raw, val_tbl, by = "raw") %>%
      tidyr::fill(paste(dt_tbl$col), .direction = "up") %>%
      dplyr::filter(!(raw %in% val_tbl$raw)) %>%
      dplyr::rename(variable = raw)

    fs_data_tbl <- fs_data %>%
      tidyr::gather(row, value, -variable) %>%
      dplyr::mutate(col = as.numeric(row)) %>%
      dplyr::left_join(dt_tbl, by = "col") %>%
      dplyr::select(date, variable, value)
  }

  fs_data_tbl

}

#' clean Yahoo data
#'
#' @param scraped_data tbl_df of scraped data
#' @param type string indicating the data type, e.g. 'IS'
#' @param ... arguments to other methods
#'
#' @return tbl_df
#'
clean_yahoo_data <- function(scraped_data, type, ...) {

  assertR::assert_true(length(type) == 1, "logic error")

  if("price" %in% type) {
    assertR::assert_present(c("daily", "weekly", "monthly", "quarterly", "annual"), frequency, "check that you specified the frequency correctly")
    clean_df <- clean_yahoo_price(scraped_data, frequency)
  } else {
    clean_df <- clean_yahoo_fs(scraped_data, type)
  }

  clean_df
}
