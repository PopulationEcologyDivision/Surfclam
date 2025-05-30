% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clamCW_QC_file.R
\name{clamCW_QC_file}
\alias{clamCW_QC_file}
\title{clamCW_QC_file
While normally run via \code{clamQC()}, this function can be used to analyze 
a single CW file.}
\usage{
clamCW_QC_file(file = NULL, layerPrefix = NULL, ...)
}
\arguments{
\item{file}{default is \code{NULL}}

\item{layerPrefix}{default is \code{NULL}}

\item{...}{Additional arguments passed on to other functions.}
}
\value{
list
}
\description{
clamCW_QC_file
While normally run via \code{clamQC()}, this function can be used to analyze 
a single CW file.
}
\examples{
\dontrun{
productFile <- clamCW_QC_file(file = "C:/Original CW Log Data/2022/2022AE Q1&Q2/Product.xlsx", 
                              rightNow = Sys.time(), 
                              layerPrefix = "Product_ex")
recordFile  <- clamCW_QC_file(file = "C:/Original CW Log Data/2022/2022AE Q1&Q2/Record.xlsx", 
                              rightNow = Sys.time(), 
                              layerPrefix = "Record_ex")
}
}
