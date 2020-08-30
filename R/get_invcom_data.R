#' navigate to ticker page in investing.com
#'
#' @param pjs_session
#' @param tkr
#'
#' @return
#'
navigate_invcom_ticker <- function(pjs_session, tkr) {

  pjs_session$go("https://za.investing.com/") # always start from the home page

  # enter ticker in searchbox and hit return
  searchElem <- pjs_session$findElement(xpath = "/html/body/div[5]/header/div[1]/div/div[3]/div[1]/input")
  searchElem$click()
  searchElem$sendKeys(tkr, webdriver::key$enter)

  # select the first element from the list of outputs
  tkrElem <- pjs_session$findElement(xpath = '//*[@id="js-main-container"]/section[1]/div/section/div/section[1]/section/div/div/div/a[1]')
  tkrElem$click()

  return(tkrElem)

}

#' Title
#'
#' @param tickers
#' @param page
#' @param frequency
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
get_invcom_data <- function(tickers, type = c("is", "bs", "cf", "price"), start_date = NULL, end_date = NULL, frequency = NULL) {

  pjs_conn <- webScrapeR::connect_session("https://za.investing.com/")
  pjs_session <- pjs_conn$session

  invcom_data_list <- list()
  for (tkr in tickers) {

    tkrElem <- navigate_invcom_ticker(pjs_session, tkr)

    # get data from overview page
    overviewDataElem <- tkrElem$findElement(css = '#js-main-container > section.main-container.container > div > section > div.e-instrument-data-and-forecast.grid-2-1 > section.common-data.show-more.clean.desktop-show-all.js-show-more > dl')
    overview_data <- overviewDataElem$getText()

    overview_raw <- stringr::str_split(overview_data[[1]], "\\n+")[[1]]

    assertR::assert_present(overview_raw, c("Open", "Shares Outstanding"))

    overview_mx <- matrix(overview_Raw[1:36], nrow = 9, ncol = 4, byrow = TRUE)

    # get data from financials page
    financialsElem <- tkrElem$findElement(xpath = '//*[@id="js-main-container"]/section[1]/div/header/div/div[3]/nav/ul/li[6]/a')
    financialsElem$click()

    invcom_data_list[[tkr]] <- overview_mx

    pjs_conn$pjs_process$kill()

  }

  invcom_data_list
}
