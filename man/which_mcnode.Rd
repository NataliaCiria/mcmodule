\name{which_mcnode}
\alias{which_mcnode}
\title{Find mcnodes in a mcmodule based on a condition}
\usage{
which_mcnode(mcmodule, test_func)
}
\arguments{
\item{mcmodule}{An mcmodule object containing node_list with mcnodes}

\item{test_func}{A function that takes an mcnode and returns a logical value
(TRUE if the condition is met)}
}
\description{
Generic function that applies a test function to each mcnode in an mcmodule
and returns the names of mcnodes where the test returns TRUE. Useful for
debugging and troubleshooting Monte Carlo models.
}
\value{
A character vector containing the names of mcnodes where test_func
returns TRUE. Returns an empty character vector if no mcnodes meet the condition.
}
\seealso{
\code{\link{which_mcnode_na}}, \code{\link{which_mcnode_inf}}
}
\examples{
# Find nodes with negative values
which_mcnode(imports_mcmodule, function(x) any(x < 0, na.rm = TRUE))

# Find nodes with values greater than 1
which_mcnode(imports_mcmodule, function(x) any(x > 1, na.rm = TRUE))
}
