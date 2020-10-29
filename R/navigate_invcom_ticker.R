#' navigate to ticker page in investing.com
#'
#' @param pjs_session phantom.js session
#' @param tkr string
#'
#' @return
#'
navigate_invcom_ticker <- function(pjs_session, tkr) {

  pjs_session$go("https://uk.investing.com/") # always start from the home page

  # enter ticker in searchbox and hit return
  searchElem <- pjs_session$findElement(xpath = '/html/body/div[5]/header/div[1]/div/div[3]/div[1]/input')
  searchElem$sendKeys(tkr, webdriver::key$enter)

  # select the first element from the list of outputs
  tkrElem <- pjs_session$findElement(xpath = '//*[@id="fullColumn"]/div/div[2]/div[2]/div[1]/a[1]')
  tkrElem$click()

  return(tkrElem)

}
