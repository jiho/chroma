#' Reformat arguments into "table" form (for color parsing functions)
#' - allow matrix/data.frame as first argument, in that case, only use it
#' - perform value recycling for vector arguments and check that they are of compatible length
#'
#' @param ... vectors of values
#' @noRd
tabularise_arguments <- function(...) {
  # get the list of arguments
  args <- list(...)

  if ( is.matrix(args[[1]]) | is.data.frame(args[[1]]) ) {
    # if first argument is a "table", use this only
    x <- args[[1]]

  } else {
    # else, convert the arguments into a "table"
    # NB: this does argument recycling by itself
    #     we just provide a nicer error message
    tryCatch(x <- data.frame(args),
      error=function(e) {
        stop("Arguments have incompatible lengths: ", paste0(sapply(args, length), collapse=", "), call.=FALSE)
      }
    )
  }

  return(x)
}


#' Check that values fall within a range
#' (in particular for color channels in color parsing functions)
#'
#' @param x vector of values to check
#' @param min,max valid range
#' @param name of the argument used for the error message (by default the name of the object provided as \code{x})
#' @noRd
is_in <- function(x, min, max, name=deparse(substitute(x))) {
  pbs <- which(x < min | x > max)
  n <- length(pbs)
  if (n > 0) {
    stop(
      name, " should be in [", min, ",", max, "]. ",
      ifelse(n == 1,
        paste("Element", pbs, "of", name, "is not."),
        paste("Elements",
          ifelse(n <= 5,
            paste0(pbs, collapse=", "),
            paste0(paste0(pbs[1:5], collapse=", "), "... and ", n-5, " others")
          ),
          "are not."
        )
      ),
      call.=FALSE
    )
  }
  return(invisible(pbs))
}


#' Insert NAs into a vector based on another one
#'
#' @param x vector without NAs.
#' @param from vector from which the NAs must be inserted in x.
#' @noRd
#' @examples
#' x <- c(1, NA, 2)
#' y <- na.omit(x)
#' na_insert(y^2, from=x)
na_insert <- function(x, from) {
  if (length(x) == length(from)) {
    x[is.na(from)] <- NA
  } else if ((length(from) - sum(is.na(from))) == length(x)) {
    x <- replace(from, !is.na(from), x)
  } else {
    stop("Incompatible number of missing values")
  }
  return(x)
}

#' Replace NAs by a given value
#'
#' @param x vector with NAs.
#' @param na.value value to replace NAs with.
#' @noRd
#' @examples
#' x <- c(1, NA, 2)
#' na_replace(x, na.value=10)
#' na_replace(x, na.value="foo")
#' na_replace(x, na.value=NA)
na_replace <- function(x, na.value) {
 if (!is.na(na.value)) {
    na_x <- is.na(x)
    if (any(na_x)) {
      x[na_x] <- na.value
    }
  }
  return(x)
}

#' Rescale within bounds
#'
#' see ?scales::rescale
#' The difference is that the values outside of the input range (from) yield the extreme of the output range (to) rather than being rescaled
#' @noRd
#' @examples
#' rescale(0:3)
#' # test out-of-bound
#' scales::rescale(0:3, from=c(1,2))
#' rescale(0:3, from=c(1,2))
#' # test reverse
#' scales::rescale(0:3, from=c(2,1))
#' rescale(0:3, from=c(2,1))
#' scales::rescale(0:3, from=c(1,2), to=c(1,0))
#' rescale(0:3, from=c(1,2), to=c(1,0))
#' scales::rescale(0:3, from=c(2,1), to=c(1,0))
#' rescale(0:3, from=c(2,1), to=c(1,0))
rescale <- function(x, to=c(0,1), from=range(x, na.rm=TRUE, finite=TRUE)) {
  x <- scales::rescale(x, to=to, from=from)
  # censor out of bounds
  if (to[1] <= to[2]) {
    x[x<to[1]] <- to[1]
    x[x>to[2]] <- to[2]
  } else {
    x[x>to[1]] <- to[1]
    x[x<to[2]] <- to[2]
  }
  return(x)
}

#' Censor based on another vector
#'
#' @param x vector to censor the values in
#' @param y vector to define the criterion on
#' @param range range of y to keep
#' @noRd
#' @examples
#' censor_from(10:20, 0:10, c(0,10))
#' censor_from(10:20, 0:10, c(1,9))
#' censor_from(1:5, c(-1, 0.5, 1, 2, NA), c(0,1))
censor <- function(x, from, range) {
  if (length(x) != length(from)) {
    stop("Cannot censor `x` based on `from` because they do not have the same length")
  }
  range <- sort(na.omit(range))
  if (length(range) != 2) {
    stop("`range` should be a valid, non-missing, range with 2 elements")
  }
  x[from < range[1] | from > range[2]] <- NA
  return(x)
}
