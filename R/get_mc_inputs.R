#' Get Monte Carlo Input Variables from Data List
#'
#' This function searches through data frames in a provided data model to find variables
#' that are inputs for Monte Carlo nodes (mcnodes) defined in a Monte Carlo table.
#'
#' @param data_keys List of lists containing data frames to search through
#' @param mctable Data frame containing Monte Carlo nodes definitions. Default is from set_mctable()
#'
#' @return Named list where each element contains column names that are MC node inputs
#'         for the corresponding data frame
#' @export
#'
#' @examples
#' mc_inputs <- get_mc_inputs(data_keys)
#' mc_inputs <- get_mc_inputs(data_keys, mctable = my_mc_table)
get_mc_inputs <- function(data_keys=set_data_keys(), mctable = set_mctable()) {
  # Get all column names from each data frame
  col_names <- lapply(data_keys, function(x) {
    if(!is.null(x$data) && is.data.frame(x$data)) {
      return(colnames(x$data))
    }
    return(NULL)
  })

  # Filter for columns that match MC node patterns
  mc_inputs<-list()
  for(i in names(col_names)) {
    data_mc_inputs_i <- grepl(paste(paste0("\\<", mctable$mcnode, ".*"),
                                    collapse = "|"),
                              col_names[[i]])
    mc_inputs[[i]][["mc_inputs"]] <- col_names[[i]][data_mc_inputs_i]
    mc_inputs[[i]][["keys"]]<-data_keys[[i]][["keys"]]
  }
  return(col_names)
}
