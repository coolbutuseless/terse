

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Default options
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
default_config <- list(
  ansi        = TRUE,
  soft        = 'grey40',
  gsep        = ', ',
  colour_nth  = 'blue4',
  colour_all  = 'grey40',
  colour_type = 'seagreen',
  nth         = 0L
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' If string exceeds with, truncate with "..." - ANSI aware.
#'
#' @param text character string
#' @param width default: 0 (auto). Set to negative for no truncation.
#'
#' @import fansi
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ellipsis <- function(text, width = 0) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Interpret 'width'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (width == 0) {
    width <- getOption('width', 80)
  } else if (width < 0) {
    width <- Inf
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Truncate if too wide
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nchar_ansi(text) > width) {
    text <- fansi::substr_ctl(text, 1, (width - 5))
    text <- paste(text, "...")
  }

  text
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Build character strings to represent the names
#'
#' @param x an object with names. Usually list or data.frame
#' @param width how wide to render the name itself?
#' @param config named list of user configuration options
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
build_names <- function(x, width = 10, config = list()) {

  config <- update_list(default_config, config)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Zero length list check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(x) == 0) {
    return('')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Figure out names of object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nn   <- names(x)
  ints <- as.character(seq_len(length(x)))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Build integer indexing i.e. "[[x]]"
  # The brackets are rendered in a different colour.
  # Index strings are padded to the maximum length
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ints <- paste0(
    ansi("[[", config$soft, use_ansi = config$ansi),
    ints,
    ansi(']]', config$soft, use_ansi = config$ansi),
    padding(width - nchar(ints) - 4)
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the names(x) is NULL, it means there are no names at all, so use the 'ints'
  # If there are some names present use them, otherwise use [[x]] indexes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(nn)) {
    nn <- ints
    idx <- rep(TRUE, length(nn))
  } else {
    idx <- is.na(nn) | nn == ''
    nn  <- ifelse(idx, ints, nn)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add whitespace padding to the RHS of each name to ensure it reaches
  # the maximum width for each depth
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nn <- ifelse(idx, pad_to_width(nn, width), pad_to_width(nn, width-1))
  nn <- ifelse(idx, nn, paste0(ansi('$', config$soft, use_ansi = config$ansi), nn))

  nn
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Output compact representation of R objects
#'
#' @param x R object
#' @param prefix prefix
#' @param width target width. 0 = auto. -1 = no limit
#' @param name_widths vector of name widths
#' @param element_widths widths to print each individual element in a vector.
#'        default: NULL (natural width), single value to apply to all elements,
#'        or a numeric vector the same length as x
#' @param max_vec_len maximum vector length anywhere in original object
#' @param config named list of user configuration options
#' \itemize{
#' \item{\code{ansi       } - Use ANSI to colour output. default: TRUE}
#' \item{\code{soft       } - non-highlight colour. default: grey40}
#' \item{\code{gsep       } - separator for vector output. default: ','}
#' \item{\code{colour_nth } - colour for every nth vector element. default: blue4}
#' \item{\code{colour_all } - colour for all other vector elements. default: grey40}
#' \item{\code{colour_type} - colour for the type/length meta information. default: seagreen}
#' \item{\code{nth        } - interval for colouring vector elements. default: 0 (off)}
#' }
#' @param ... other arguments
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terse <- function(x, prefix = '', width = 0, name_widths = NULL,
                  element_widths = NULL, max_vec_len = NULL, config = list()) {
  UseMethod('terse')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Output a single line representation of an object
#'
#' @inheritParams terse
#' @param single_line Representation of R object in a single line
#' @param vec_type single character to represent class of object
#' @param vec_len length of this object
#' @param max_vec_len maximum length of all objects to be be presented.
#' @param config user config options in a named list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terse_core <- function(single_line, vec_type, vec_len, prefix = NULL, width = 0,
                       name_widths = NULL, max_vec_len = 99, config = list()) {

  config <- update_list(default_config, config)

  pad <- ifelse(is.null(name_widths), "", padding(sum(name_widths)))
  len <- pad_to_width(vec_len, nchar(as.character(max_vec_len)))

  res <- paste0(
    prefix,
    pad,
    " ",
    ansi(paste0(vec_type, len), config$colour_type, use_ansi = config$ansi),
    " ",
    single_line
  )
  res <- ellipsis(res, width = width)
  cat(res, "\n", sep="")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a single line by collapsing an atomic and highlighting every
# nth value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_single_line <- function(x, element_widths = NULL, config = list()) {

  config <- update_list(default_config, config)

  if (!is.null(element_widths)) {
    # pad elements to fit the specified element widths
    stopifnot(length(element_widths) == 1 || length(element_widths) == length(x))
    x <- mapply(pad_to_width, x, element_widths, MoreArgs = list(align='l'))
    x <- unlist(x)
  } else if (!is.null(names(x))) {
    nn <- names(x)
    x  <- ifelse(is.na(nn) | nn=='', x, paste0(nn, '=', x))
  }
  x <- colour_vec(vec = x, n = config$nth, colour_all = config$colour_all,
                  colour_nth = config$colour_nth, use_ansi = config$ansi)
  paste(x, collapse = config$gsep)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname terse
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terse.default <- function(x, prefix = NULL, width = 0, name_widths = NULL,
                          element_widths = NULL, max_vec_len = 100, config = list()) {

  config <- update_list(default_config, config)

  if (is.matrix(x)) {
    single_line <- apply(x, 2, function(x) {paste0("[", paste(x, collapse=", "), "]")})
    single_line <- paste(single_line, collapse = "")
    dim_text    <- paste0("(", paste(dim(x), collapse=","), ")")
    single_line <- paste(dim_text, single_line)
    vec_type    <- 'm'
  } else if (is.array(x)) {
    single_line <- paste0("[", paste(x, collapse=", "), "]")
    dim_text    <- paste0("(", paste(dim(x), collapse=","), ")")
    single_line <- paste(dim_text, single_line)
    vec_type    <- 'a'
  } else if (is.logical(x)) {
    x           <- ifelse(x, 'T', 'F')
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'l'
  } else if (is.factor(x)) {
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'f'
  } else if (inherits(x, 'POSIXct')) {
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'p'
  } else if (inherits(x, 'POSIXlt')) {
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'P'
  } else if (inherits(x, 'Date')) {
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'D'
  } else if (is.name(x)) {
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'n'
  } else if (is.integer(x)) {
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'i'
  } else if (is.double(x)) {
    x <- round(x, 5)
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'd'
  } else if (is.raw(x)) {
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'r'
  } else if (is.character(x)) {
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'c'
  } else if (is.complex(x)) {
    single_line <- create_single_line(x, element_widths = element_widths, config = config)
    vec_type    <- 'C'
  } else {
    single_line <- long_deparse(x)
    single_line <- ansi(single_line, config$soft, use_ansi = config$ansi)
    if (is.null(x)) {
      vec_type <- 'N'
    } else if (is.call(x)) {
      vec_type <- 'K'
    } else if (is.function(x)) {
      vec_type <- 'F'
    } else if (is.environment(x)) {
      vec_type <- 'E'
    } else if (is.expression(x)) {
      vec_type <- 'e'
    } else if (is.list(x)) {
      vec_type <- 'L'
      single_line <- long_deparse(unname(x))
      single_line <- ansi(single_line, config$soft, use_ansi = config$ansi)
    } else {
      vec_type <- 'x'
    }
  }

  terse_core(
    single_line,
    prefix      = prefix,
    vec_type    = vec_type,
    vec_len     = length(x),
    max_vec_len = max_vec_len,
    width       = width,
    name_widths = name_widths,
    config      = config
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname terse
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terse.data.frame <- function(x, prefix = NULL, width = 0, name_widths = NULL,
                             element_widths = NULL, max_vec_len = NULL,
                             config = list(), ...) {

  config <- update_list(default_config, config)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this is the root call (where prefix is NULL), then try and use the name
  # of the data that is passed in as the first prefix in the output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(prefix)) {
    prefix <- as.character(substitute(x))
    if (length(prefix) > 1) {
      prefix <- ""
    } else {
      prefix <- ansi(prefix, config$soft, use_ansi = config$ansi)
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this is the root call, then work out the maximum length of the names
  # at each depth level. This is used for spacing out names in the prefix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(name_widths)) {
    name_widths <- max_name_length(x)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get the names of each column, padded to the appropriate width for this
  # level. Then concatenate prefix+nn
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nn     <- build_names(x, width = name_widths[1], config = config)
  prefix <- paste0(prefix, nn)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # terse output for all elements.
  # If this is a data.frame then we want to use terse.default to ensure that
  # lists are printed as single line.
  # You're on your own if you try and nest something weird in a data.frame,
  # but the default should at least not be 100% terrible
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.data.frame(x)) {
    terse_func     <- terse.default
    element_widths <- calc_max_element_widths(x)
    max_vec_len    <- max_vec_len %||% nrow(x)
  } else {
    element_widths <- NULL
    terse_func     <- terse
    max_vec_len    <- max_vec_len %||% max_length(x)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # terse output for all columns in this data.frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(x) == 0) {
    terse.default(x, prefix,
                  width          = width,
                  name_widths    = name_widths,
                  element_widths = element_widths,
                  max_vec_len    = max_vec_len,
                  config         = config
    )
  } else {
    res <- mapply(terse_func, x, prefix,
                  MoreArgs = list(
                    width          = width,
                    name_widths    = name_widths[-1],
                    element_widths = element_widths,
                    max_vec_len    = max_vec_len,
                    config         = config
                  )
    )
  }

  invisible()
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname terse
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terse.list <- terse.data.frame

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname terse
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terse.defaultlist <- terse.list

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname terse
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terse.lm <- terse.list


if (FALSE) {

  library(dplyr)
  # library(terse)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  terse(list(list(), list(1, 2, 3)))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  terse(mtcars)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- list(1, 2, davy_gones = 3)
  terse(ll)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  library(magrittr)
  list(1, 2, davy_jones = 3) %>% terse()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  terse(list(1, 2, davy_gones = 3))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- list(a = list(b = 1, 2))
  terse(ll)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- as.list(letters)
  terse(ll)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- list(
    a = list(b = c(letters[1:5], NA), c = list(f = c(12.1, 13.2, 3, 0))),
    d = letters,
    e = as.raw(1:5),
    five = setNames(1:26, letters)
  )

  terse(ll)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- list(
    letters,
    mtcars,
    c = list(d = c(yes = TRUE, no = FALSE), e = list(f = 2:4), r = as.raw(1:10))
  )

  terse(ll)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- list(
    letters,
    iris,
    c = list(d = c(TRUE, FALSE), e = list(f = 2:4), r = as.raw(1:10))
  )

  terse(ll)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- list(
    letters,
    list()
  )

  terse(ll)


}














