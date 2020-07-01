

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extend a vector to the given length by appending the given value
#'
#' @param vec numeric vector
#' @param len desired length
#' @param value value to append to shorter vectors
#'
#' @return vector of the requested length
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extend_to_length <- function(vec, len, value = -Inf) {
  stopifnot(is.numeric(vec))
  stopifnot(length(vec) <= len)
  c(vec, rep(value, len - length(vec)))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Version of pmax which doesn't recycle, but instead extends each vector
#'
#' @param ... multiple numeric vector arguments
#'
#' @return pmax across all vector without recycling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aligned_pmax <- function(...) {
  vecs <- list(...)
  lens <- lengths(vecs)
  vecs <- lapply(vecs, extend_to_length, len = max(lens), value = -Inf)
  do.call(pmax, vecs)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine maximum name lengths at all depths
#'
#' @param x object, usually a list or data.frame
#'
#' @return vector of the maximum name length at each depth
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
max_name_length <- function(x) {
  if (!is.list(x) && !is.data.frame(x)) {
    return(0)
  }

  # don't recurse any ruther into data.frames
  if (is.data.frame(x)) {
    return(max(nchar(names(x))) + 1L)
  }

  # zero length lists
  if (length(x) == 0) {
    return(0)
  }

  brackets <- 4  # chars for =  '[[]]'

  nx <- names(x)
  if (is.null(nx)) {
    max_len <- brackets + nchar(as.character(length(x)))
  } else {
    lens <- nchar(nx) + 1  # account for $ sign
    idx  <- which(lens == 1)
    lens[idx] <- nchar(as.character(idx)) + brackets
    max_len <- max(lens)
  }

  sub_lens <- (lapply(x, max_name_length))
  sub_lens <- do.call(aligned_pmax, sub_lens)

  c(max_len, sub_lens)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine maximum lengths of any object anywhere in list or data.frame
#'
#' @param x object, usually a list or data.frame
#'
#' @return maximum element length
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
max_length <- function(x) {
  if (is.atomic(x)) {
    return(length(x))
  }

  if (length(x) == 0) {
    return(0)
  }

  if (!is.list(x) && !is.data.frame(x)) {
    return(0)
  }

  if (is.data.frame(x)) {
    return(nrow(x))
  }

  max_len <- max(lengths(x))

  sub_lens <- vapply(x, max_length, numeric(1))

  max(max_len, sub_lens)
}





if (FALSE) {
  x <- list(
    appletini  = 1,
    bob    = list(robert = 1, robbie = 2),
    greg   = list(thomas_tank = list(sam = 1))
  )

  max_name_length(x)

  max_length(x)


  x <- list(
    1,
    list(robert = 1, robbie = 2),
    list(thomas_tank = list(sam = 1))
  )

  max_name_length(x)

}
