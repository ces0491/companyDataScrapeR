#' get financial statement data from investing.com
#'
#' @param pjs_session phantom.js session
#' @param type string indicating the financial statement required. one of 'IS', 'BS' or 'CFS'
#'
#' @return tbl_df
#'
#' @importFrom magrittr %>%
#' @export
#'
get_invcom_fs_data <- function(pjs_session, type) {

  fin_data_elem <- pjs_session$findElement(xpath = '//*[@id="pairSublinksLevel1"]/li[3]/a')
  fin_data_elem$click()

  xpath <- dplyr::case_when(type == "IS" ~ '//*[@id="pairSublinksLevel2"]/li[2]/a',
                            type == "BS" ~ '//*[@id="pairSublinksLevel2"]/li[3]/a',
                            type == "CFS" ~ '//*[@id="pairSublinksLevel2"]/li[4]/a')

  fin_st_elem <- pjs_session$findElement(xpath = xpath)
  fin_st_elem$click()

  # we want annual financial statements
  annual_btn <- pjs_session$findElement(xpath = '//*[@id="leftColumn"]/div[8]/div[1]/a[1]')
  annual_btn$click()

  pjs_session$waitFor('ExpectedConditions.visibilityOfElementLocated(By.id("rrtable"))')

  tbl_elem <- pjs_session$findElement(xpath = '//*[@id="rrtable"]/table')
  tbl_raw <- tbl_elem$getText()

  tmp_df <- data.frame(do.call(cbind, strsplit(tbl_raw, "\n", fixed = TRUE)))

  dt_header <- tmp_df[1:8, ][c(1,3,5,7)]
  dt_header <- stringr::str_extract(dt_header, '[0-9]+')
  n_data_cols <- length(dt_header)

  fs_df <- tmp_df %>%
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
    dplyr::mutate(value = as.numeric(value))

  fs_df_long

}
