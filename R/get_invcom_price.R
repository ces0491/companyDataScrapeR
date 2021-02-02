#' get historical price data from investing.com
#'
#' @param pjs_session phantom.js session
#' @param start_dt start date
#' @param end_dt end date
#'
#' @return tbl_df
#'
#' @importFrom magrittr %>%
#'
pvt_get_invcom_price_data <- function(pjs_session, start_dt, end_dt) {

  assertR::assert_true(!is.null(start_dt), "Attempting to retrieve price data from Investing.com, please specify a start date")

  start_dt <- as.character(format(as.Date(start_dt), "%d/%m/%Y"))
  if(is.null(end_dt)) end_dt <- Sys.Date()
  end_dt <- as.character(format(as.Date(end_dt), "%d/%m/%Y"))

  gen_data_elem <- pjs_session$findElement(xpath = '//*[@id="pairSublinksLevel1"]/li[1]')
  gen_data_elem$click()

  hist_data_elem <- pjs_session$findElement(xpath = '//*[@id="pairSublinksLevel2"]/li[3]/a')
  hist_data_elem$click()

  date_rng_elem <- pjs_session$findElement(xpath = '//*[@id="widgetFieldDateRange"]')
  date_rng_elem$click()

  start_dt_elem <- pjs_session$findElement(xpath = '//*[@id="startDate"]')
  start_dt_elem$clear()
  start_dt_elem$sendKeys(start_dt)
  end_dt_elem <- pjs_session$findElement(xpath = '//*[@id="endDate"]')
  end_dt_elem$clear()
  end_dt_elem$sendKeys(end_dt)

  apply_elem <- pjs_session$findElement(xpath = '//*[@id="applyBtn"]')
  apply_elem$click()

  Sys.sleep(1) # wait for the table to load

  tbl_elem <- pjs_session$findElement(xpath = '//*[@id="results_box"]')
  tbl_raw <- tbl_elem$getText()

  price_df <- data.frame(do.call(cbind, strsplit(tbl_raw, "\n", fixed = TRUE)))

  price_df
}

#' Get price data from investing.com
#'
#' @param pjs_session phantom.js session
#' @param start_dt start date
#' @param end_dt end date
#'
#' @return tbl_df
#'
get_invcom_price_data <- function(pjs_session, start_dt, end_dt) {

  if(is.null(pjs_session)) {
    price_df <- NA
  } else {
    price_df <- pvt_get_invcom_price_data(pjs_session, start_dt, end_dt)
  }

  price_df
}
