#' Lightness scale and palette
#'
#' Lightness-based color scale and palette in HCL space.
#'
#' @template param_lightness
#' @template param_chromacity
#' @template param_hue
#' @inheritParams interp_scale
#'
#' @template details_hcl
#'
#' @template return_scales
#'
#' @seealso \code{\link{luminance}} for a the computation of perceived luminance and the creation of perception-based luminance palettes.
#' @template seealso_hcl_scales
#' @family color scales and palettes
#'
#' @examples
#' # Define a dark-to-light blue scale
#' blues <- light_scale(h=220)
#' # and apply it to some data
#' blues(x=c(0, 0.2, 0.6, 1))
#'
#' # Define a palette function
#' # (which works like the actual rainbow() function)
#' blues_pal <- light_palette(h=220, c=0.3)
#' # and get 10 colors from it
#' blues_pal(n=10)
#' show_col(blues_pal(n=10))
#' # or use the shortcut
#' show_col(light_colors(n=50, h=220, c=0.3))
#'
#' # Determine hue from a color
#' blues <- light_colors(n=50, h="dodgerblue")
#' greens <- light_colors(n=50, h="green")
#' yellows <- light_colors(n=50, h="gold")
#' pinks <- light_colors(n=50, h="deeppink")
#' show_col(blues, greens, yellows, pinks)
#'
#' # Perceived lightness increases similarly among hues,
#' # which makes the different palettes comparable
#' # (this would not be the case with a HSL or HSV gradient)
#' plot(  luminance(blues),   col=blues[40])
#' points(luminance(greens),  col=greens[40])
#' points(luminance(yellows), col=yellows[40])
#' points(luminance(pinks),   col=pinks[40])
#'
#' # Lightness scales are good for continuous variables
#' # such as the elevation of the Maunga Whau volcano
#' image(maunga, col=light_colors(100, h=140))
#' contour(maunga, col=alpha("white", 0.5), add=TRUE)
#'
#' persp(maunga, theta=50, phi=25, border=alpha("black", 0.3),
#'       col=light_map(persp_facets(maunga$z), h=140))
#'
#' \dontrun{library("rgl")
#' persp3d(maunga, aspect=c(1,0.6,0.3), axes=FALSE, box=FALSE,
#'         col=light_map(maunga$z, h=140))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#' }
#' # With a limited number of levels, they can also work for discrete variables
#' attach(iris)
#' plot(Petal.Length, Sepal.Length, pch=19, col=light_map(Species))
#' legend(1, 8, legend=levels(Species), pch=19,
#'        col=light_colors(n=nlevels(Species)))
#' # but a hue-based scale is probably more appropriate (see ?hue_map)
#'
#' @importFrom scales rescale
#' @export
light_scale <- function(l=c(0,0.9), c=0.5, h=0, domain=c(0,1), reverse=FALSE) {
  # check arguments
  if (length(l) != 2) {
    stop("l needs to be a vector of length 2, defining the minimum and maximum lightness to use.")
  }

  # define the function
  f <- function(x) {
    # define colors
    colors <- hcl(h=h, c=c, l=scales::rescale(x, from=domain, to=l))
    if (reverse) {
      colors <- rev(colors)
    }
    return(colors)
  }
  return(f)
}

#' @param ... passed to \code{\link{light_scale}}. Note that argument \code{domain} is meaningless in functions other than \code{light_scale} and passing it through \code{...} is an error.
#' @rdname light_scale
#' @export
light_map <- function(x, ...) {
  # force characters into factors to be able to convert them to numeric
  if (is.character(x)) {
    x <- factor(x)
  }
  # convert to numbers
  x <- as.numeric(x)
  # define the domain of the scale
  light_scale(domain=range(x, na.rm=T), ...)(x)
}

#' @rdname light_scale
#' @export
light_palette <- function(...) {
  f <- function(n) {
    light_scale(domain=c(0,1), ...)(seq(0, 1, length.out=n))
  }
  return(f)
}

#' @rdname light_scale
#' @export
light_colors <- function(n, ...) {
  light_palette(...)(n)
}
