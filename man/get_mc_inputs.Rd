% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_node_list.R
\name{get_mc_inputs}
\alias{get_mc_inputs}
\title{Get Monte Carlo Input Variables from Data Keys}
\usage{
get_mc_inputs(data_keys = set_data_keys(), mctable = set_mctable())
}
\arguments{
\item{data_keys}{List of lists containing data frames to search through (default: set_data_keys())}

\item{mctable}{Data frame containing Monte Carlo nodes definitions (default: set_mctable())}
}
\value{
Named list where each element contains column names that are MC node inputs
for the corresponding data frame
}
\description{
Searches through data frames in a provided data model (data_keys) to find variables
that are inputs for Monte Carlo nodes (mcnodes) defined in a Monte Carlo table (mctable).
}
\examples{
\dontrun{
imports_mc_inputs <- get_mc_inputs(imports_data_keys, mctable = imports_mctable)
}
}
