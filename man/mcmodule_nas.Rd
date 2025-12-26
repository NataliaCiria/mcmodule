\name{mcmodule_nas}
\alias{mcmodule_nas}
\title{Find mcnodes in a mcmodule that contain NAs}
\usage{
mcmodule_nas(mcmodule)
}
\arguments{
\item{mcmodule}{An mcmodule object containing node_list with mcnodes}
}
\description{
This function identifies which mcnodes within an mcmodule contain NA values.
This is useful for troubleshooting and debugging Monte Carlo models to find
nodes that may be causing issues due to missing or undefined values.
}
\value{
A character vector containing the names of mcnodes that have NA values.
Returns an empty character vector if no NAs are found.
}
\seealso{
\code{\link{mcnode_na_rm}} for replacing NA values in mcnodes
}
\examples{
\dontrun{
# Find nodes with NAs in the imports_mcmodule
mcmodule_nas(imports_mcmodule)
}
}
