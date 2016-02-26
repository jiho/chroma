#' Color scales and palettes
#'
#' Create a color scale, create a color palette, get a few colors from a palette, or map values to colors along a scale.
#'
#' @param colors vector of colors specified as hex strings or named R colors. By default, those colors will be evenly distributed along the scale and new colors will be interpolated between them.
#' @param model color space in which to perform the interpolation; valid models are \code{lab} (the default and usually most suitable), \code{rgb}, \code{hsv}, \code{hsl}, \code{hcl}, \code{lch}. Beware that all but \code{lab} and \code{rgb} can give surprising results.
#' @param interp type of interpolation to perform; either \code{linear} (the default) or \code{bezier}, which results in a smoother transition between colors. \code{bezier} interpolation is only available with \code{model="lab"} however.
#' @param domain the values between which the scale is computed.
#' @param reverse whether to reverse the order of colors along the scale.
#' @param values if colours should not be evenly positioned along the gradient this vector gives the position for each color in the \code{colors} vector. This argument supersedes \code{domain} and \code{reverse} because it defines the bounds and direction of the color scale.
#'
#' @template return_scales
#'
#' @examples
#' # define a color scale
#' coldhot_scale <- make_scale(c("cornflowerblue", "brown3"))
#' # apply it to some data
#' coldhot_scale(c(0, 0.2, 0.6, 1))
#' # for values outside the range, the extreme color of scale is returned
#' coldhot_scale(1.3)
#' 
#' # define a palette
#' coldhot_pal <- make_palette(c("cornflowerblue", "brown3"))
#' # get 10 colors from it
#' coldhot_pal(10)
#' show_col(coldhot_pal(10))
#' 
#' # shortcut to define a palette and extract n colors from it
#' show_col(make.colors(n=50, colors=c("cornflowerblue", "brown3")))
#' 
#' # test interpolation spaces and types
#' cols <- c("yellow", "blue", "red")
#' show_col(
#'    make_palette(cols, model="lab")(10),
#'    make_palette(cols, model="lab", interp="bez")(10),
#'    make_palette(cols, model="rgb")(10),
#'    make_palette(cols, model="hsv")(10),
#'    make_palette(cols, model="hcl")(10)
#' )
#' 
#' # change mapping
#' x <- 0:10
#' cols <- c("aliceblue", "cornflowerblue", "dodgerblue4")
#' show_col(
#'    make_scale(cols)(x),
#'    make_scale(cols, domain=range(x))(x),
#'    make_scale(cols, domain=range(x), reverse=TRUE)(x),
#'    make_scale(cols, values=c(0,1,10))(x)
#' )
#'
#' @export
make_scale <- function(colors=c("white", "black"), model="lab", interp="linear", domain=c(0,1), reverse=FALSE, values=NULL) {
  # force input R colors into hex notation
  colors <- in_hex(colors)
  
  # check arguments
  model <- match.arg(model, c("lab", "hcl", "lch", "hsl", "hsv", "rgb"))
  interp <- match.arg(interp, c("bezier", "linear"))
  if (interp == "bezier" & model != "lab") {
    warning("Bezier interpolation can only be done in L*a*b* space; switching to model=\"lab\".")
  }
  
  # define domain
  if ( ! is.null(values) ) {
    # check content
    if (!is.numeric(values)) {
      stop("Argument 'values' should be a numeric vector.")
    }
    if (any(!is.finite(values))) {
      warning("Argument 'values' should not contain missing or non-numeric values. They were removed.")
      values <- values[is.finite(values)]
    }
    if ( ! ( identical(sort(values), values) | identical(sort(values), rev(values)) ) )  {
      stop("Numbers in 'values' should be monotonously increasing or decreasing.")
    }
    if ( length(values) != length(colors) )  {
      stop("Not the same number of 'colors' (",length(colors),") and 'values' (", length(values), ").")
    }
    domain <- values
  } else {
    if ( reverse ) {
      domain <- rev(domain)
    }
  }

  # prepare chroma.js command
  domain <- paste0("[",paste0(domain, collapse=","),"]")  
  colors <- paste0("['", paste0(colors, collapse="','"), "']")
  if ( interp == "linear" ) {
    interp <- "scale"
  }
  
  # define the scale function which calls chroma.js internally
  eval(f <- function(x) {
    cmds <- paste0("chroma.",interp,"(",colors,")",ifelse(interp=="bezier", ".scale()", ""),".domain(",domain,").mode('", model, "')(", x, ").hex()")
    chroma:::v8_eval(cmds)
  })
  # TODO return NA outside of range
  # TODO "cheat" to be faster: get 50 or 100 colors from chroma.js and use colorRamp on those to compute the actual colors.
  # n=100000; system.time(grDevices::rgb(colorRamp(cols)((0:n)/n), max=255))

  return(f)
}

#' @param ... passed to \code{\link{make_scale}}. Note that \code{domain} and \code{values} are meaningless in functions other than \code{make_scale} and passing them in \code{...} is an error.
#' @name make_scale
#' @export
make_palette <- function(...) {
  f <- function(n) {
    make_scale(domain=c(0,1), values=NULL, ...)(seq(0, 1, length.out=n))
  }
  return(f)
}

#' @param n number of colors to extract from the color palette.
#' @name make_scale
#' @export
make_colors <- function(n, ...) {
  make_palette(...)(n)
}
#' @name make_scale
#' @export
make.colors <- make_colors

#' @param x a vector whose values will be coerced to numbers and mapped to colors.
#' @name make_scale
#' @export
make_map <- function(x, ...) {
  # force characters into factors to be able to convert them to numeric
  if (is.character(x)) {
    x <- factor(x)
  }
  # convert to numbers
  x <- as.numeric(x)
  # define the domain of the scale
  colors <- make_scale(domain=range(x, na.rm=T), ...)(x)
  return(colors)
}
