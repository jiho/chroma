#' Lightness scale and palette
#'
#' Lightness-based color scale and palette in HCL space.
#'
#' @param chroma chromacity, vector of two numbers in \code{[0,~1]} (0 is grey, ~1 is full color) giving the minimum and maximum chromacities along the scale.
#' @inheritParams interp_scale
#' @inheritParams hcl
#'
#' @template details_hcl
#'
#' @template return_scales
#'
#' @template seealso_hcl_scales
#' @family color scales and palettes
#'
#' @export
#'
#' @examples
#' # Define a scale towards a more intense red
#' reds <- chroma_scale(h=30)
#' # and apply it to some data
#' reds(x=c(0, 0.2, 0.6, 1))
#' show_col(reds(x=c(0, 0.2, 0.6, 1)))
#'
#' # Define a palette function
#' reds_pal <- chroma_palette(h=30)
#' # and get 10 colors from it
#' reds_pal(n=10)
#' show_col(reds_pal(n=10))
#' # or use the shortcut and get 50 colors
#' show_col(chroma_colors(n=50, h=30))
#'
#' # Determine hue from a color and then define a chroma scale
#' blues <- chroma_colors(n=50, h="dodgerblue")
#' greens <- chroma_colors(n=50, h="green")
#' yellows <- chroma_colors(n=50, h="gold")
#' pinks <- chroma_colors(n=50, h="deeppink")
#' show_col(blues, greens, yellows, pinks)
#'
#' # Chroma scales can be used for continuous variables
#' # such as the elevation of the Maunga Whau volcano
#' image(maunga, col=chroma_colors(100, h="red"))
#' contour(maunga, col=alpha("white", 0.5), add=TRUE)
#'
#' filled.contour(maunga, color.palette=chroma_palette(h="red"))
#'
#' persp(maunga, theta=50, phi=25, border=alpha("black", 0.3),
#'       col=chroma_map(persp_facets(maunga$z), h="red"))
#'
#' \dontrun{
#' library("rgl")
#' persp3d(maunga, aspect=c(1,0.6,0.3), axes=FALSE, box=FALSE,
#'         col=chroma_map(maunga$z, h="red"))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#' }
#' # but a lightness-based scale would be probably be better (see ?light_scale)
#' # With a limited number of levels, they may also work for discrete variables
#' attach(iris)
#' plot(Petal.Length, Sepal.Length, pch=19, col=chroma_map(Species))
#' legend(1, 8, legend=levels(Species), pch=19,
#'        col=chroma_colors(n=nlevels(Species)))
#' # but a hue-based scale is probably more appropriate (see ?hue_map)
chroma_scale <- function(chroma=c(0,1), l=0.5, h=0, domain=c(0,1), reverse=FALSE) {
  # NB: argument is named `chroma` to avoid conflict with `c` (error: promise already under evaluation). But the `c` abbreviation works.

  # check arguments
  if (length(chroma) != 2) {
    stop("chroma needs to be a vector of length 2, defining the minimum and maximum chroma to use.")
  }
  # define the function
  f <- function(x) {
    # define colors
    colors <- hcl(h=h, c=scales::rescale(x, from=domain, to=chroma), l=l)
    if (reverse) {
      colors <- rev(colors)
    }
    return(colors)
  }
  return(f)
}

#' @param ... passed to \code{\link{chroma_scale}}. Note that argument \code{domain} is meaningless in functions other than \code{chroma_scale} and passing it through \code{...} is an error.
#' @rdname chroma_scale
#' @export
chroma_map <- function(x, ...) {
  # force characters into factors to be able to convert them to numeric
  if (is.character(x)) {
    x <- factor(x)
  }
  # convert to numbers
  x <- as.numeric(x)
  # define the domain of the scale
  chroma_scale(domain=range(x, na.rm=T), ...)(x)
}

#' @rdname chroma_scale
#' @export
chroma_palette <- function(...) {
  f <- function(n) {
    chroma_scale(domain=c(0,1), ...)(seq(0, 1, length.out=n))
  }
  return(f)
}

#' @rdname chroma_scale
#' @export
chroma_colors <- function(n, ...) {
  chroma_palette(...)(n)
}
