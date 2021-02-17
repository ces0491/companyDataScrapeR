#' get price data from Yahoo
#'
#' @param url string specifying the download link for the price data
#'
#' @return tbl_df with columns, Date, Open, High, Low, Close, Adj.Close, Volume
#'
get_yahoo_price_data <- function(url) {

  download_dir <- glue::glue("C:/temp/chromeDL/yahoo_price_data")
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  destfile <- glue::glue("{download_dir}/temp_yahoo_price.csv") # add extension to file name for path

  tmp_file <- try(utils::download.file(url, destfile, mode = "wb"), silent = TRUE)

  if (inherits(tmp_file, "try-error")) {
      price_tbl <- tibble::tibble("Date" = NA, "Open" = NA, "High" = NA, "Low" = NA, "Close" = NA, "Adj.Close" = NA, "Volume" = NA)

    } else {
      raw_price <- utils::read.csv(destfile, stringsAsFactors = FALSE)
      price_tbl <- tibble::tibble(raw_price)

      fileR::clear_files(clear_dir = download_dir, file_name = "temp_yahoo_price.csv") # clearing temp file
    }

  price_tbl
}
