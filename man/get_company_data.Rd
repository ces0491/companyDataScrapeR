% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_company_data.R
\name{get_company_data}
\alias{get_company_data}
\title{Get company data from Yahoo Finance or Investing.com}
\usage{
get_company_data(tickers, type, start_date, end_date, frequency)
}
\arguments{
\item{tickers}{character vector of tickers where data is prefixed with the source, e.g. YAH-EXAMPLETICKER}

\item{type}{string specifying the type of data required - one of is, bs, cfs or price}

\item{start_date}{start date}

\item{end_date}{end date}

\item{frequency}{string indicating the frequency to return price data, e.g. 'monthly'}
}
\value{
object of class \code{tbl_df} with columns, ticker, type and nested raw and clean company data
}
\description{
Get company data from Yahoo Finance or Investing.com
}
