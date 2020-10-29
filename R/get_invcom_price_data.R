#' get historical price data from investing.com
#'
#' @param pjs_session phantom.js session
#' @param start_dt start date
#' @param end_dt end date
#'
#' @return tbl_df
#' @export
#'
get_invcom_price_data <- function(pjs_session, start_dt, end_dt) {

  assertR::assert_true(!is.null(start_dt), "Attempting to retrieve price data from Investing.com, please specify a start date")

  start_dt <- as.character(format(as.Date(start_dt), "%d/%m/%Y"))
  if(is.null(end_dt)) end_dt <- as.character(format(Sys.Date(), "%d/%m/%Y"))

  hist_data_elem <- pjs_session$findElement(xpath = '//*[@id="pairSublinksLevel2"]/li[3]/a')
  hist_data_elem$click()

  date_rng_elem <- pjs_session$findElement(xpath = '//*[@id="widgetFieldDateRange"]')
  date_rng_elem$click()
  pjs_session$takeScreenshot()

  start_dt_elem <- pjs_session$findElement(xpath = '//*[@id="startDate"]')
  start_dt_elem$clear()
  start_dt_elem$sendKeys(start_dt)
  end_dt_elem <- pjs_session$findElement(xpath = '//*[@id="endDate"]')
  end_dt_elem$clear()
  end_dt_elem$sendKeys(end_dt)

  apply_elem <- pjs_session$findElement(xpath = '//*[@id="applyBtn"]')
  apply_elem$click()

  tbl_elem <- pjs_session$findElement(xpath = '//*[@id="curr_table"]')
  tbl_raw <- tbl_elem$getText()

  tmp_df <- data.frame(do.call(cbind, strsplit(tbl_raw, "\n", fixed = TRUE)))
  headers <- strsplit(tmp_df[1,], "\\s")[[1]][1:7]

  price_df <- tmp_df %>%
    dplyr::rename("all_string" = 1) %>%
    dplyr::filter(dplyr::row_number() != 1) %>%
    dplyr::mutate(date_str = substr(all_string, 1, 13)) %>%
    dplyr::mutate(price_str = stringr::str_replace_all(all_string, date_str, "")) %>%
    tidyr::separate(price_str, into = headers[2:7], sep = "\\s") %>%
    dplyr::select(date_str, Price, Open, High, Low) %>%
    dplyr::mutate(day = substr(date_str, 5, 6)) %>%
    dplyr::mutate(month = substr(date_str, 1, 3)) %>%
    dplyr::mutate(year = substr(date_str, 9, 12)) %>%
    tidyr::unite(date, year, month, day, sep = "/") %>%
    dplyr::mutate(date = as.Date(date, "%Y/%b/%d")) %>%
    dplyr::select(date, Price, Open, High, Low)

  price_df
}
