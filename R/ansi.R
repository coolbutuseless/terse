

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Reset back to terminal defaults
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reset <- "\033[39m\033[49m"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an R colour to a 216-colour ANSI string. Suitable for most terminals
#'
#' Ref: \url{https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit}
#'
#' If all the RGB colour components are equal then the colour is matched to one
#' of 24 grey levels, other wise it is converted to one of 216 standard colours.
#'
#' @param rcolour any R colour e.g. 'red', '#445566'
#'
#' @return ANSI escape string for the given colour as a foreground or background
#'         colour
#'
#' @importFrom grDevices col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2bg <- function(rcolour) {
  if (is.null(rcolour)) {return('')}
  code <- col2code(rcolour)
  paste0("\033[48;5;", code, "m")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname col2bg
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2fg <- function(rcolour) {
  if (is.null(rcolour)) {return('')}
  code <- col2code(rcolour)
  paste0("\033[38;5;", code, "m")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname col2bg
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2code <- function(rcolour) {
  cols <- col2rgb(rcolour)

  is_grey        <- cols[1,] == cols[2,] & cols[2,] == cols[3,]
  possibly_white <- cols[1,] == 255L

  grey_code <- 232L + as.integer(round(cols[1,]/255 * 23))

  cols <- round(cols/255 * 5)
  colour_code <- 16L + 36L * cols[1,] + 6 * cols[2,] + cols[3,]

  ifelse(is_grey & !possibly_white, grey_code, colour_code)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Colour a string with ANSI
#'
#' @param x character string
#' @param fg,bg foreground and background colours. NULL for nothing
#' @param sep seperator for collapseing strings.
#' @param use_ansi default: TRUE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ansi <- function(x, fg = NULL, bg = NULL, sep = "", use_ansi) {
  if (!isTRUE(use_ansi)) { return(x) }
  res <- ifelse(is.null(fg) && is.null(bg), '', reset)
  paste0(col2fg(fg), col2bg(bg), x, res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Colour and output a string with ANSI
#'
#' @inheritParams ansi
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cansi <- function(x, fg = NULL, bg = NULL, sep = "", use_ansi = TRUE) {
  cat(ansi(x, fg, bg, sep, use_ansi = use_ansi))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Remove ansi codes from a string
#'
#' @param x character string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
strip_ansi <- function(x) {
  gsub("\033.*?m", '', x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Remove ansi codes from a string and calculate nchar()
#'
#' @param x character string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nchar_ansi <- function(x) {
  nchar(strip_ansi(x))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Colour a vector using a highlight for every nth element
#'
#' @param vec atomic which can be coerced to character
#' @param n the interval to highlight. Set to 0 to disable highlighting
#' @param colour_all ansi colour of all elements
#' @param colour_nth ansi colour applied every nth element
#' @param use_ansi default: TRUE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colour_vec <- function(vec, n, colour_all, colour_nth, use_ansi) {

  if (is.na(n) || n < 1) {
    return(ansi(vec, colour_all, use_ansi = use_ansi))
  }

  ifelse(
    (seq_along(vec) - 1) %% n == 0,
    ansi(vec, colour_nth, use_ansi = use_ansi),
    ansi(vec, colour_all, use_ansi = use_ansi)
  )
}








