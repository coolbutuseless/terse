

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate the width of the character representation of each element in a vector
#'
#' If the given vec is not actually atomic, just pretend it has a width of 0.
#'
#' Treat NAs as 2 characters wide
#'
#' @param vec presumed atomic vector which is a column in a data.frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_element_widths_vec <- function(vec) {
  if (!is.atomic(vec)) {
    return(rep_len(0L, length(vec)))
  }

  lens <- nchar(as.character(vec))
  lens[is.na(lens)] <- 2L

  lens
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate the maximum element width for each row of a data.frame
#'
#' @param df data.frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_max_element_widths <- function(df) {
  stopifnot(is.data.frame(df))

  col_widths <- lapply(df, get_element_widths_vec)

  do.call(pmax, col_widths)
}
