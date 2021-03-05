#' get financial statement data from Yahoo
#'
#' @param pjs_session phantom.js session
#' @param url string specifying the url for the financial statement of interest
#'
#' @return single column tbl of raw scraped text data
#'
get_yahoo_fs_data <- function(pjs_session, url) {

  pjs_session <- pjs_session$go(url)

  fs_tbl_elem <- try(pjs_session$findElement(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/div[1]'), silent = TRUE) #//*[@id="Col1-1-Financials-Proxy"]/section/div[4]/div[1]/div[1]

  if (inherits(fs_tbl_elem, "try-error")) {
    fs_tbl <- tibble::tibble(raw = NA)

    } else {

    fs_tbl_raw <- fs_tbl_elem$getText()
    fs_tbl_txt <- unlist(strsplit(fs_tbl_raw[[1]], "\n"))
    fs_df <- data.frame(raw_text = fs_tbl_txt)

    units_elem <- pjs_session$findElement(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[2]/span/span')
    units_raw <- units_elem$getText()

    fs_tbl <- data.frame(raw_text = units_raw) %>%
      dplyr::bind_rows(fs_df)
  }

  fs_tbl
}


