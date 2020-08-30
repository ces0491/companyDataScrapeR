#' clean summary data tables from Yahoo
#'
#' @param scraped_summ_data tbl_df
#'
#' @return tbl_df
#'
clean_yahoo_summary <- function(scraped_summ_data) {

  assertR::assert_true(length(scraped_summ_data) == 2, "expecting list of 2")

  sum_data_tbl <- scraped_summ_data[[1]] %>%
    dplyr::bind_rows(scraped_summ_data[[2]]) %>%
    dplyr::rename(variable = X1,
                  value = X2) %>%
    dplyr::mutate(value = gsub(",", "", value)) %>%
    dplyr::mutate(value = trimws(value)) %>%
    dplyr::mutate(value = ifelse(value == "N/A" | value == "N/A (N/A)", NA, value))

  sum_data_tbl
}


#' clean Yahoo data
#'
#' @param scraped_data tbl_df
#' @param type string
#' @param ... arguments to other methods
#'
#' @return tbl_df
#'
clean_yahoo_data <- function(scraped_data, type, ...) {

  assertR::assert_true(length(type) == 1, "logic error")

  if("price" %in% type) {
    assertR::assert_true(!is.null(frequency), "You've requested price data, please specify a frequency")
    clean_df <- clean_yahoo_price(scraped_data, frequency)
  } else {
    clean_df <- clean_yahoo_fs(scraped_data, type)
  }

  clean_df
}
