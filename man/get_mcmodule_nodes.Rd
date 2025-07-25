% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval_module.R
\name{get_mcmodule_nodes}
\alias{get_mcmodule_nodes}
\title{Get Nodes from Monte Carlo Module}
\usage{
get_mcmodule_nodes(mcmodule, mc_names = NULL, envir = parent.frame())
}
\arguments{
\item{mcmodule}{An mcmodule or mcnode_list object}

\item{mc_names}{Optional vector of node names to retrieve}

\item{envir}{Environment where MC nodes will be created (default: parent.frame())}
}
\value{
A subset of the node list containing requested nodes
}
\description{
Retrieves nodes from a Monte Carlo module and assigns them to the parent environment
}
