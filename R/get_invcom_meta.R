#' Get meta data for security from investing.com
#'
#' @param pjs_session phantom.js session
#'
#' @return data.frame with columns variable of class character and value of class character
#'
pvt_get_invcom_meta_data <- function(pjs_session) {

  reqd_vars <- c("Beta", "Market Cap", "Shares Outstanding")

  # clickable links always have href as an attribute. If not clickable, we are already on the page
  financials_elem <- pjs_session$findElement(linkText = 'Financials')
  if(!is.null(financials_elem$getAttribute('href'))) {
    financials_elem$click()
  }

  units_elem <- pjs_session$findElement(xpath = '//*[@id="leftColumn"]/div[13]')
  units_raw <- units_elem$getText()

  gen_data_elem <- pjs_session$findElement(linkText = 'General')
  if(!is.null(gen_data_elem$getAttribute('href'))) {
    gen_data_elem$click()
  }

  name_elem <- pjs_session$findElement(xpath = '//*[@id="leftColumn"]/div[1]/h1')
  name_raw <- name_elem$getText()

  fx_elem <- pjs_session$findElement(xpath = '//*[@id="quotes_summary_current_data"]/div[1]/div[1]/div[2]')
  fx_raw <- fx_elem$getText()

  gen_info_elem <- pjs_session$findElement(xpath = '//*[@id="quotes_summary_current_data"]/div[2]')
  general_info <- gen_info_elem$getText()

  tbl_elem <- pjs_session$findElement(xpath = '//*[@id="leftColumn"]/div[9]')
  tbl_raw <- tbl_elem$getText()

  # clean

  ticker <- stringr::str_extract_all(name_raw,  "(?<=\\().+?(?=\\))")[[1]] # extract string in parentheses
  name <- stringr::str_remove(name_raw, ticker)
  name_clean <- stringr::str_sub(name, end = -4) # remove last 3 characters - parentheses and space

  name_df <- data.frame(variable = "Name", value = name_clean)

  fx <- stringr::str_extract(string = fx_raw, pattern = "(?<=\\.).*(?=\\()")
  fx_clean <- stringr::str_sub(fx, start = -4)
  fx_df <- data.frame(variable = "Currency", value = trimws(fx_clean))

  info_split <- strsplit(general_info, "\n", fixed = TRUE)[[1]]
  market_raw <- stringr::str_subset(info_split, "Market")
  market <- stringr::str_remove(market_raw, "Market:")
  market_clean <- trimws(market, "both")
  market_df <- data.frame(variable = "Market", value = market_clean)

  summ_tbl <- data.frame(do.call(cbind, strsplit(tbl_raw, "\n", fixed = TRUE)))

  rep_units_df <- data.frame(variable = "Reporting Units", value = units_raw)

  clean_summ <- summ_tbl %>%
    dplyr::rename(variable = 1) %>%
    dplyr::mutate(reqd_var = variable %in% reqd_vars) %>%
    dplyr::mutate(reqd_val = dplyr::lag(reqd_var)) %>%
    dplyr::mutate(filt = ifelse(reqd_var != reqd_val, TRUE, FALSE)) %>%
    dplyr::filter(filt) %>%
    dplyr::select(variable)

  order_vec <- c("Name", "Market", "Currency", "Market Cap", "Shares Outstanding", "Beta", "Reporting Units")

  meta_df <- clean_summ %>%
    dplyr::mutate(value = dplyr::lead(variable)) %>%
    dplyr::filter(dplyr::row_number() %% 2 == 1) %>% ## Select odd rows
    dplyr::bind_rows(name_df) %>%
    dplyr::bind_rows(fx_df) %>%
    dplyr::bind_rows(market_df) %>%
    dplyr::bind_rows(rep_units_df) %>%
    dplyr::slice(match(order_vec, variable)) %>% # reorder rows according to order vec
    data.frame(.)

  meta_df
}

#' get ticker meta data from investing.com
#'
#' @param pjs_session phantom.js session
#'
#' @return tbl_df
#'
get_invcom_meta_data <- function(pjs_session) {

  if(is.null(pjs_session)) {
    meta_df <- NA
  } else {
    meta_df <- pvt_get_invcom_meta_data(pjs_session)
  }

  meta_df
}

