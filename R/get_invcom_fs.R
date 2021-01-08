#' get financial statement data from investing.com
#'
#' @param pjs_session phantom.js session
#' @param type string indicating the financial statement required. one of 'IS', 'BS' or 'CFS'
#'
#' @return tbl_df
#'
#' @importFrom magrittr %>%
#'
get_invcom_fs_data <- function(pjs_session, type) {

  assertR::assert_true(length(type) == 1, 'logic error - only specify 1 type at a time')

  fin_data_elem <- pjs_session$findElement(linkText = 'Financials')
  fin_data_elem$click()

  xpath <- dplyr::case_when(type == "IS" ~ '//*[@id="pairSublinksLevel2"]/li[2]/a',
                            type == "BS" ~ '//*[@id="pairSublinksLevel2"]/li[3]/a',
                            type == "CFS" ~ '//*[@id="pairSublinksLevel2"]/li[4]/a')

  fin_st_elem <- pjs_session$findElement(xpath = xpath)
  fin_st_elem$click()

  # we want annual financial statements
  annual_btn <- pjs_session$findElement(xpath = '//*[@id="leftColumn"]/div[8]/div[1]/a[1]')
  annual_btn$click()

  # pjs_session$waitFor('ExpectedConditions.visibilityOfElementLocated(By.id("rrtable"))')
  Sys.sleep(1)

  tbl_elem <- pjs_session$findElement(xpath = '//*[@id="rrtable"]/table')
  tbl_raw <- tbl_elem$getText()

  fs_df <- data.frame(do.call(cbind, strsplit(tbl_raw, "\n", fixed = TRUE)))

  fs_df

}
