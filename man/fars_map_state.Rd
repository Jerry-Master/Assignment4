% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Represent the data}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An integer that represent a state}

\item{year}{An integer of the wanted year}
}
\value{
A map with relevant points.
}
\description{
A function that will plot in a map the point correspondents to the most affected
places, or a message if there are no accidents.
}
\examples{
\dontrun{
fars_map_state(1, 2013)
fars_map_state(30, 2015)}

}
