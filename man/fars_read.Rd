% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read data}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string giving the pathway to the data file}
}
\value{
This function returns a data frame called data with a file given.
}
\description{
This is a function that will read a csv file and transform it into a
data frame tbl (using \code{dplyr::tbl_df(data)}) only if the pathway
given is valid (\code{!file.exists(filename)})
}
\examples{
\dontrun{
data_2014<-fars_read("accident_2014.csv.bz2")
}
}
