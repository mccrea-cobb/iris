% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_required_data.R
\name{get_cost_codes}
\alias{get_cost_codes}
\title{Get refuge cost codes}
\usage{
get_cost_codes(saveit = FALSE, filedir)
}
\arguments{
\item{saveit}{a logical value indicating whether the the returned data frame should be saved}

\item{filedir}{a connection or the name of the file where the data will be saved}
}
\value{
a data frame containing refuge names and their associated cost codes for Alaska refuges
}
\description{
Creates a data frame of cost codes for refuges in the Alaska Region.
These are used for querying station-specific data.
}
\examples{
\dontrun{
get_cost_codes(TRUE, "cc.Rdata")
}
}
