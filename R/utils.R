
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NULL operator from dplyr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Deparse an object to a single string
#'
#' base::deparse has a maximum width of 500 characters before breaking into
#' multiple strings. This wrapper around deparse just concatenates these
#' multiple strings
#'
#' @param x object
#'
#' @return single character string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
long_deparse <- function(x) {
  paste(trimws(deparse(x, width.cutoff = 500)), collapse = " ")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pad strings to a given width
#'
#' @param x character strings
#' @param len length
#' @param align align text to the left or right? default: left
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pad_to_width <- function(x, len, align = c('l', 'r')) {
  align <- match.arg(align)

  # Build the format string
  if (align == 'l') {
    fstring <- paste0("%-", len, 's')
  } else {
    fstring <- paste0("%", len, 's')
  }

  # Apply the format string
  sprintf(fstring, x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate a character vector of padding strings
#'
#' @param lens lengths
#'
#' @return vector of character strings containing blanks of the given lengths
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
padding <- function(lens) {
  lens[lens < 0] <- 0
  vapply(
    lens,
    function(x) { paste(rep(' ', x), collapse = "") },
    character(1)
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Basic version of modifyList stolen from ggplot2
#'
#' @param old,new named lists
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_list <- function (old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}



