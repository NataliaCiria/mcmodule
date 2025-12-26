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
test_mcnode_na <- mcdata(c(0.1, NA, 0.3), type = "0", nvariates = 3)
test_mcnode_clean <- mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3)
test_mcmodule <- list(
  node_list = list(
    node_a = list(mcnode = test_mcnode_na),
    node_b = list(mcnode = test_mcnode_clean)
  )
)
which_mcnode_na(test_mcmodule)
}
