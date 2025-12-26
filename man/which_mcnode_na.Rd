\name{which_mcnode_na}
\alias{which_mcnode_na}
\title{Find mcnodes in a mcmodule that contain NAs}
\usage{
which_mcnode_na(mcmodule)
}
\arguments{
\item{mcmodule}{An mcmodule object containing node_list with mcnodes}
}
\description{
Identifies which mcnodes within an mcmodule contain NA values.
Useful for troubleshooting and debugging Monte Carlo models to find
nodes that may be causing issues due to missing or undefined values.
}
\value{
A character vector containing the names of mcnodes that have NA values.
Returns an empty character vector if no NAs are found.
}
\seealso{
\code{\link{which_mcnode}}, \code{\link{which_mcnode_inf}}, \code{\link{mcnode_na_rm}}
}
\examples{
# Find nodes with NAs in the imports_mcmodule
which_mcnode_na(imports_mcmodule)

# Create a test mcmodule with NAs
test_mcmodule <- list(
  node_list = list(
    node_a = list(mcnode = c(1, 2, NA, 4)),
    node_b = list(mcnode = c(5, 6, 7, 8))
  )
)
which_mcnode_na(test_mcmodule)
}
