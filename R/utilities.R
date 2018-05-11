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


#' Censor values based on another vector
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


#' Post-process the output of a scale
#'
#' replace NAs by their code
#' remove values out of domain
#'
#' @noRd
post_process_scale <- function(colors, na.value, extrapolate, x, domain) {
  # replace NAs by na.value when necessary
  colors <- na_replace(colors, na.value)

  # remove out of domain values
  if (!extrapolate) { colors <- censor(colors, from=x, range=domain) }

  return(colors)
}


#' Coerce various things to numbers
#'
#' chroma.js deals with numbers everywhere so it is safer to just convert everything
#'
#' @noRd
as.num <- function(x, ...) {
  # force characters into factors to be able to convert them to numeric
  if (is.character(x)) { x <- factor(x) }
  # convert to numbers
  x <- as.numeric(x, ...)
  return(x)
}

#' Convert a scale function into a map one
#'
#' @noRd
as_map <- function(scalefun, x, ...) {
  # convert to number
  x <- as.num(x)
  # define the domain automatically and appy the scale
  scalefun(domain=range(x, na.rm=TRUE, finite=TRUE), ...)(x)
}
as_palette <- function(scalefun, ...) {
  # prepare a function that computes n colors
  f <- function(n) {
    scalefun(domain=c(1,n), ...)(1:n)
  }
  return(f)
}


#' N-dimensional interpolation on a grid
#'
#' @param x list of vectors of coordinates of the grid.
#' @param V array or dimensions defined by x containing the values to interpolate.
#' @param xo coordinates of the points at which to interpolate; either a vector to interpolate a single point or a table (coerced to a matrix) with one point per line to interpolate several.
#' @noRd
#'
#' @examples
#' # 2D
#' x <- list(0:1, 0:1)
#' V <- array(c(0, 0.1, 0.1, 0.2), dim=c(2,2))
#' xo <- cbind(c(0.75, 0.5, 0.1), c(0.5, 0.5, 0.4))
#' interpn(x, V, xo)
#'
#' # 3D
#' x <- seq(-1,1,l=5)
#' V <- outer(outer(x, x), x)
#' x <- list(x, x, x)
#' xo <- cbind(c(0.75, 0.5, 0.1), c(0.5, 0.5, 0.4), c(-0.3, -0.3, -0.3))
#' interpn(x, V, xo)
interpn <- function(x, V, xo) {
  # checks
  if (any(sapply(x, length) != dim(V))) {
    stop("x and V are or incompatible dimensions")
  }
  n <- length(x)
  if (is.vector(xo)) {
    xo <- matrix(xo, nrow=1)
  } else {
    xo <- as.matrix(xo)
  }
  if (ncol(xo) != n) {
    stop("The dimensions of xo are incompatible with x and V.")
  }
  apply(xo, 1, interpn1, x=x, V=V)
}
interpn1 <- function(xo, x, V) {
  n <- length(x)

  # find the hypercube in which xo is and define its relative coordinates within it
  # each coordinate is in % or the cube's side = in [0,1]
  ix <- xo
  for (j in 1:n) {
    ix[j] <- approx(x[[j]], 1:length(x[[j]]), xo[j])$y
  }

  # extract the corners of the hypercube
  i0 <- floor(ix)
  i1 <- ceiling(ix)
  i <- lapply(1:n, function(j) {c(i0[j], i1[j])})
  i <- as.matrix(expand.grid(i))

  # compute the weights for the values in each corner
  ix <- ix - i0
  p <- lapply(1:n, function(j) {c(1-ix[j], ix[j])})
  p <- expand.grid(p)
  p <- apply(p,1,prod)

  # compute the interpolated value
  yo <- sum(V[i]*p)

  return(yo)
}

# # Python reference
# import scipy.interpolate as int
# import numpy as np
# import copy
#
# # 2D
# points = ([0,1],[0,1])
# values = np.array([[0., 0.1], [0.1, 0.2]])
# xi = [[0.75, 0.5],
#       [0.5, 0.5],
#       [0.1, 0.4]]
# int.interpn(points, values, xi)
#
# # 3D
# x = np.linspace(-1,1,5)
# points = (x, x, x)
# x1 = copy.deepcopy(x)
# x2 = copy.deepcopy(x)
# x3 = copy.deepcopy(x)
# x1.shape = (5,1,1)
# x2.shape = (1,5,1)
# x3.shape = (1,1,5)
# values = (x1 * x2) * x3
# xi = [[0.75, 0.5, -0.3],
#       [ 0.5, 0.5, -0.3],
#       [ 0.1, 0.4, -0.3]]
# int.interpn(points, values, xi)


