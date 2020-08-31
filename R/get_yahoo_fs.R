#' get financial statement data from Yahoo
#'
#' @param pjs_session phantom.js session
#'
#' @return single column tbl of raw scraped text data
#'
get_yahoo_fs_data <- function(pjs_session) {

  fs_tbl_elem <- try(pjs_session$findElement(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[4]/div[1]/div[1]'), silent = TRUE)

  if (inherits(fs_tbl_elem, "try-error")) {
    fs_tbl <- tibble::tibble(raw = NA)

    } else {

    fs_tbl_raw <- fs_tbl_elem$getText()
    fs_tbl_txt <- unlist(strsplit(fs_tbl_raw[[1]], "\n"))

    fs_tbl <- tibble::tibble(raw = fs_tbl_txt)
  }

  fs_tbl
}


