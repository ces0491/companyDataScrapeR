#' Get meta data for security from investing.com
#'
#' @param pjs_session phantom.js session
#'
#' @return data.frame with columns variable of class character and value of class character
#'
get_invcom_meta_data <- function(pjs_session) {

  reqd_vars <- c("Beta", "Market Cap", "Shares Outstanding")

  gen_data_elem <- pjs_session$findElement(xpath = '//*[@id="pairSublinksLevel1"]/li[1]')
  gen_data_elem$click()

  name_elem <- pjs_session$findElement(xpath = '//*[@id="leftColumn"]/div[1]/h1')
  name_raw <- name_elem$getText()

  tbl_elem <- pjs_session$findElement(xpath = '//*[@id="leftColumn"]/div[9]')
  tbl_raw <- tbl_elem$getText()

  # clean

  ticker <- stringr::str_extract_all(name_raw,  "(?<=\\().+?(?=\\))")[[1]] # extract string in parentheses
  name <- stringr::str_remove(name_raw, ticker)
  name_clean <- stringr::str_sub(name, end = -4) # remove last 3 characters - parentheses and space

  name_df <- data.frame(variable = "name", value = name_clean)

  summ_tbl <- data.frame(do.call(cbind, strsplit(tbl_raw, "\n", fixed = TRUE)))

  clean_summ <- summ_tbl %>%
    dplyr::rename(variable = 1) %>%
    dplyr::mutate(reqd_var = variable %in% reqd_vars) %>%
    dplyr::mutate(reqd_val = dplyr::lag(reqd_var)) %>%
    dplyr::mutate(filt = ifelse(reqd_var != reqd_val, TRUE, FALSE)) %>%
    dplyr::filter(filt) %>%
    dplyr::select(variable)

  meta_df <- clean_summ %>%
    dplyr::mutate(value = dplyr::lead(variable)) %>%
    dplyr::filter(dplyr::row_number() %% 2 == 1) %>% ## Select odd rows
    dplyr::bind_rows(name_df)

  meta_df
}
