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

#' clean financial statement tables from Yahoo
#'
#' @param scraped_fs_data tbl_df
#' @param type string
#'
#' @return tbl_df
#'
clean_yahoo_fs <- function(scraped_fs_data, type) {

  if (all(is.na(scraped_fs_data$raw)) | length(scraped_fs_data$raw) < 4) {

    fs_data_tbl <- tibble::tibble(date = as.Date(NA), variable = as.character(NA), value = as.numeric(NA))

  } else {

    dt_tbl <-  scraped_fs_data %>%
      dplyr::filter(grepl("\\/", raw)) %>%
      dplyr::mutate(split_1 = strsplit(raw, "\\/")) %>%
      tidyr::unnest(split_1) %>%
      dplyr::mutate(year = substring(split_1, 1,4)) %>%
      dplyr::mutate(year = ifelse(grepl("TTM", split_1), format(Sys.Date(), "%Y"), year)) %>%
      dplyr::filter(nchar(year) == 4) %>%
      dplyr::mutate(raw2 = gsub("TTM", "", raw)) %>%
      dplyr::mutate(month = sub("\\/.*", "", raw2)) %>%
      dplyr::mutate(month = ifelse(grepl("TTM", split_1), as.numeric(format(Sys.Date(), "%m"))-1, month)) %>%
      dplyr::mutate(day = 1) %>%
      tidyr::unite(date_str, year,month,day, sep = "-") %>%
      dplyr::mutate(date = dateR::get_eom_dates(as.Date(date_str))) %>%
      dplyr::mutate(col = dplyr::row_number()) %>%
      dplyr::select(raw, raw2, date, col)

    cnt_dt <- length(dt_tbl$date)

    # scraped data will always begin with 'Breakdown' (the variable column header) and the dates
    reqd_raw <- scraped_fs_data %>%
      dplyr::filter(raw != "Breakdown") %>%
      dplyr::filter(raw != dt_tbl$raw[1])

    val_str <- reqd_raw %>%
      dplyr::mutate(value = stringr::str_split(raw, "(?<!-)\\s")) %>% # split string either on space or a hyphen followed by a space (negative numbers)
      tidyr::unnest(value) %>%
      dplyr::mutate(value = gsub("\\s", "", value)) %>% # remove any spaces that may be left as a result of negative numbers
      dplyr::mutate(value = gsub(",", "", value)) %>%
      dplyr::filter(stringr::str_detect(value, "\\d+")) %>% # hyphens may be left behind. just keep the numbers before converting
      dplyr::mutate(dbl_hyph = substr(value, 1, 2)) %>% # sometimes a blank cell (denoted with a hyphen) exists before a negative value and so a double hyphen prefix occurs
      dplyr::mutate(value = ifelse(dbl_hyph == "--", substring(value, 2), value)) %>%
      dplyr::select(-dbl_hyph)

    val_tbl <- val_str %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      dplyr::select(raw, value) %>%
      dplyr::group_by(raw) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(row, value) %>% # annoyingly, the values get reordered after the spread so rejoin reqd raw by raw to get the order back
      dplyr::select(1:(cnt_dt + 1))

    fs_data <- dplyr::left_join(reqd_raw, val_tbl, by = "raw") %>%
      tidyr::fill(paste(dt_tbl$col), .direction = "up") %>%
      dplyr::filter(!(raw %in% val_tbl$raw)) %>%
      dplyr::rename(variable = raw)

    fs_data_tbl <- fs_data %>%
      tidyr::gather(row, value, -variable) %>%
      dplyr::mutate(col = as.numeric(row)) %>%
      dplyr::left_join(dt_tbl, by = "col") %>%
      dplyr::select(date, variable, value)
  }

  fs_data_tbl

}
