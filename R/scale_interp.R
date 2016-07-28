#' Interpolated color scales and palettes
#'
#' Interpolate between colors to create a color scale, map values to colors along a scale, create a color palette, or get a few colors from a palette.
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
#' @family color scales and palettes
#'
#' @examples
#' # Define a color scale
#' coldhot_scale <- color_scale(c("cornflowerblue", "brown3"))
#' # Apply it to some data
#' coldhot_scale(c(0, 0.2, 0.6, 1))
#' # For values outside the range, the extreme color of scale is returned
#' coldhot_scale(1.3)
#' 
#' # Define a palette
#' coldhot_pal <- color_palette(c("cornflowerblue", "brown3"))
#' # and get 10 colors from it
#' coldhot_pal(10)
#' show_col(coldhot_pal(10))
#' # Use the shortcut to define a palette and extract n colors from it
#' show_col(interp.colors(n=50, colors=c("cornflowerblue", "brown3")))
#' 
#' # Test interpolation spaces and types
#' cols <- c("yellow", "blue", "red")
#' show_col(
#'    color_palette(cols, model="lab")(10),
#'    color_palette(cols, model="lab", interp="bez")(10),
#'    color_palette(cols, model="rgb")(10),
#'    color_palette(cols, model="hsv")(10),
#'    color_palette(cols, model="hcl")(10)
#' )
#' 
#' # Change mapping region/direction
#' x <- 0:10
#' cols <- c("aliceblue", "cornflowerblue", "dodgerblue4")
#' show_col(
#'    color_scale(cols)(x),
#'    color_scale(cols, domain=range(x))(x),
#'    color_scale(cols, domain=range(x), reverse=TRUE)(x),
#'    color_scale(cols, values=c(0,1,10))(x)
#' )
#'
#' # Maunga Whau volcano colors picked from a picture
#' # (likely incorrect perceptually but attempts a "realistic" look)
#' maunga <- c("#C4B99F", "#282A19", "#61781B", "#BC9352")
#' image(maunga, col=interp_colors(100, colors=maunga))
#' # = the dark ring-like level is indeed misleading
#'
#' persp(maunga, theta=50, phi=25, border=alpha("black", 0.3),
#'       col=color_map(volcano[-1,-1], colors=maunga))
#' # NB: This is cheating, colouring each facet according to the value of
#' #     its lower right point. The correct way is \code{link{persp_facets}}
#' persp(maunga, theta=50, phi=25, border=alpha("black", 0.3),
#'       col=color_map(persp_facets(volcano), colors=maunga))
#'
#' \dontrun{library("rgl")
#' persp3d(maunga, aspect=c(1,0.6,0.3), axes=FALSE, box=FALSE,
#'         col=color_map(volcano, colors=maunga))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#' }
#' # Color points according to a discrete variable
#' attach(iris)
#' plot(Petal.Length, Sepal.Length, pch=21, cex=2, bg=color_map(Species))
#' legend(1, 8, legend=levels(Species), pch=21, pt.bg=interp_colors(n=nlevels(Species)))
#' # NB: works, but a continuous scale is not really appropriate here.
#'
#' @export
#' @importFrom scales rescale
#' @importFrom grDevices rgb colorRamp
color_scale <- function(colors=c("white", "black"), model="lab", interp="linear", domain=c(0,1), reverse=FALSE, values=NULL) {
  # force input R colors into hex notation
  colors <- in_hex(colors)
  
  # check arguments
  model <- match.arg(model, c("lab", "hcl", "lch", "hsl", "hsv", "rgb"))
  interp <- match.arg(interp, c("bezier", "linear"))
  if (interp == "bezier" & model != "lab") {
    warning("Bezier interpolation can only be done in L*a*b* space; switching to model=\"lab\".")
    model <- "lab"
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
  domaint <- paste0("[",paste0(domain, collapse=","),"]")  
  colorst <- paste0("['", paste0(colors, collapse="','"), "']")
  if ( interp == "linear" ) {
    interp <- "scale"
  }
  
  # define the scale function which calls chroma.js internally
  eval(f <- function(x) {
    # for small data, call chroma.js directly
    if (length(x) < 100) {
      cmds <- paste0("chroma.",interp,"(",colorst,")",ifelse(interp=="bezier", ".scale()", ""),".domain(",domaint,").mode('", model, "')(", x, ").hex()")
      colors <- v8_eval(cmds)
    }
    # for large data, cheat: use chroma.js to get a few colors and interpolate the new ones with colorRamp which is faster
    else {
      # get 100 colors
      xx <- seq(domain[1], domain[2], length.out=100)
      cmds <- paste0("chroma.",interp,"(",colorst,")",ifelse(interp=="bezier", ".scale()", ""),".domain(",domaint,").mode('", model, "')(", xx, ").hex()")
      colors <- v8_eval(cmds)
      # interpolate between them
      # NB: colorRamp works between 0 and 1 only
      colors <- grDevices::colorRamp(colors, space="Lab", interpolate="linear")(scales::rescale(x, from=domain))
      # convert them to hex
      colors <- grDevices::rgb(colors, maxColorValue=255)
    }
    return(colors)
  })
  # TODO return NA outside of range

  return(f)
}

#' @param ... passed to \code{\link{color_scale}}. Note that arguments \code{domain} and \code{values} are meaningless in functions other than \code{color_scale} and passing them through \code{...} is an error.
#' @param x a vector whose values will be coerced to numbers and mapped to colors.
#' @name color_scale
#' @export
color_map <- function(x, ...) {
  # force characters into factors to be able to convert them to numeric
  if (is.character(x)) {
    x <- factor(x)
  }
  # convert to numbers
  x <- as.numeric(x)
  # define the domain of the scale
  colors <- color_scale(domain=range(x, na.rm=TRUE), ...)(x)
  return(colors)
}

#' @name color_scale
#' @export
color_palette <- function(...) {
  f <- function(n) {
    color_scale(domain=c(0,1), values=NULL, ...)(seq(0, 1, length.out=n))
  }
  return(f)
}

#' @param n number of colors to extract from the color palette.
#' @name color_scale
#' @export
interp_colors <- function(n, ...) {
  color_palette(...)(n)
}
#' @name color_scale
#' @export
interp.colors <- interp_colors

