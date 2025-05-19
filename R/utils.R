#' Add Group IDs to Data Frames
#'
#' @param x First dataset
#' @param by Grouping variables
#' @param y Optional second dataset
#' @return Dataframe or list of dataframes with added group IDs
#' @import dplyr
#' @examples
#' \dontrun{
#'add_group_id(imports_data, by = c("pathogen ", "origin"))
#' }
add_group_id <- function(x, y = NULL, by = NULL) {
  if (!is.null(y)) {
    if (is.null(by)) {
      # Get categorical variables for each dataframe
      cat_x <- names(x)[sapply(x, function(col) is.character(col) | is.factor(col))]
      cat_y <- names(y)[sapply(y, function(col) is.character(col) | is.factor(col))]

      # Find intersection of categorical variables
      by <- intersect(cat_x, cat_y)
      message("Group by: ", paste(by, collapse = ", "))
    }

    if (!all(by %in% names(x))) {
      stop(paste0(by[by %in% names(x)], " columns not found in ", deparse(substitute(x))))
    }
    if (!all(by %in% names(y))) {
      stop(paste0(by[by %in% names(y)], " columns not found in ", deparse(substitute(y))))
    }

    x$df <- "x"
    y$df <- "y"

    xy <- dplyr::bind_rows(x[c(by, "df")], y[c(by, "df")]) %>%
      dplyr::mutate(
        g_id = NULL,
        g_row = NULL
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::mutate(g_id = dplyr::cur_group_id())

    x <- xy %>%
      dplyr::filter(df == "x") %>%
      dplyr::bind_cols(x[!names(x) %in% c(by, "df", "g_id", "g_row")]) %>%
      dplyr::mutate(
        df = NULL,
        g_row = dplyr::cur_group_rows()
      ) %>%
      dplyr::relocate(g_id, g_row) %>%
      dplyr::ungroup()

    y <- xy %>%
      dplyr::filter(df == "y") %>%
      dplyr::bind_cols(y[!names(y) %in% c(by, "df", "g_id", "g_row")]) %>%
      dplyr::mutate(
        df = NULL,
        g_row = dplyr::cur_group_rows()
      ) %>%
      dplyr::relocate(g_id, g_row) %>%
      dplyr::ungroup()

    return(list(x = x, y = y))
  } else {
    x <- x %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::mutate(
        g_id = dplyr::cur_group_id(),
        g_row = dplyr::cur_group_rows()
      ) %>%
      dplyr::relocate(g_id, g_row) %>%
      dplyr::ungroup()

    return(x)
  }
}


#' Match and align keys between two datasets
#'
#' @param x First dataset containing keys to match
#' @param y Second dataset containing keys to match
#' @param keys_names Names of columns to use as matching keys. If NULL, uses common columns
#' @return List containing:
#'   \item{x}{First dataset with group IDs}
#'   \item{y}{Second dataset with group IDs}
#'   \item{xy}{Matched datasets with aligned group and scenario IDs}
#' @import dplyr
#' @examples
#' \dontrun{
#' x <- data.frame(type = 1:2, scenario_id = c(0, 1))
#' y <- data.frame(type = 1:2, scenario_id = c(0, 2))
#' keys_match(x, y, keys_names = "type")
#' }
keys_match <- function(x, y, keys_names = NULL) {
  # Add common group ids
  keys_list <- add_group_id(x, y, keys_names)
  keys_x <- keys_list$x
  keys_y <- keys_list$y

  # Define keys_names if not provided
  if (is.null(keys_names)) {
    keys_names <- unique(intersect(names(keys_x), names(keys_y)))
    keys_names <- keys_names[!keys_names %in% c("g_id", "g_row", "scenario_id")]
  }

  # Group and scenario matching
  keys_xy <- keys_x %>%
    dplyr::full_join(keys_y, by = c("g_id", "scenario_id", keys_names)) %>%
    dplyr::relocate("g_id", "scenario_id", dplyr::all_of(keys_names))

  # Get group ids for baseline scenario (scenario_id = 0)
  keys_xy_0 <- keys_xy %>%
    dplyr::full_join(keys_y, by = c("g_id", "scenario_id", keys_names)) %>%
    dplyr::filter(scenario_id == 0) %>%
    dplyr::transmute(
      g_id,
      g_row.x_0 = g_row.x,
      g_row.y_0 = g_row.y
    )

  # Fill in missing values using baseline scenario
  keys_xy <- keys_xy %>%
    dplyr::left_join(keys_xy_0, by = "g_id") %>%
    dplyr::mutate(
      g_row.x = ifelse(is.na(g_row.x), g_row.x_0, g_row.x),
      g_row.x_0 = NULL,
      g_row.y = ifelse(is.na(g_row.y), g_row.y_0, g_row.y),
      g_row.y_0 = NULL
    )

  return(list(
    x = keys_x,
    y = keys_y,
    xy = keys_xy
  ))
}
