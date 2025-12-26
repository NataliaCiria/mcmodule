\name{which_mcnode_inf}
\alias{which_mcnode_inf}
\title{Find mcnodes in a mcmodule that contain infinite values}
\usage{
which_mcnode_inf(mcmodule)
}
\arguments{
\item{mcmodule}{An mcmodule object containing node_list with mcnodes}
}
\description{
Identifies which mcnodes within an mcmodule contain infinite values (Inf or -Inf).
Useful for troubleshooting and debugging Monte Carlo models to find
nodes that may be causing issues due to infinite values.
}
\value{
A character vector containing the names of mcnodes that have infinite values.
Returns an empty character vector if no infinite values are found.
}
\seealso{
\code{\link{which_mcnode}}, \code{\link{which_mcnode_na}}, \code{\link{mcnode_na_rm}}
}
\examples{
# Find nodes with infinite values in the imports_mcmodule
which_mcnode_inf(imports_mcmodule)

# Create a test mcmodule with Inf values
test_mcnode_inf <- mcdata(c(0.1, Inf, 0.3), type = "0", nvariates = 3)
test_mcnode_clean <- mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3)
test_mcmodule <- list(
  node_list = list(
    node_a = list(mcnode = test_mcnode_inf),
    node_b = list(mcnode = test_mcnode_clean)
  )
)
which_mcnode_inf(test_mcmodule)
}
