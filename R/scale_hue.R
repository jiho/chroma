#' Hue scale and palette
#'
#' Hue-based color scale and palette in HCL space.
#'
# @param h range of hues to use, a vector of length 2 with either angles around the color wheel, in \code{[0, 360]} (angles outside of the range are rotated back to within \code{[0, 360]}: 380 = 20, -15 = 345, etc.), or colors (hex or named) from which the hue is extracted (by function \code{\link{hue}}).
#' @template param_hue
#' @template param_chromacity
#' @template param_lightness
#' @inheritParams make_scale 
#'
#' @template details_hcl
#'
#' @template return_scales
#'
#' @template seealso_hcl_scales
#'
#' @examples
#' # Define a perceptually-correct "rainbow"-like scale function
#' rainbow_scale <- hue_scale()
#' # and apply it to some data
#' rainbow_scale(x=c(0, 0.2, 0.6, 1))
#' 
#' # Define a palette function
#' # (which works like the actual rainbow() function)
#' rainbow_pal <- hue_palette()
#' # and get 10 colors from it
#' rainbow_pal(n=10)
#' show_col(rainbow_pal(n=10))
#' # or use the shortcut
#' hue_colors(n=50)
#' show_col(hue_colors(n=50))
#' 
#' # Palettes of varying hue but constant chromacity and lightness
#' # are appropriate to distinguish among levels of a discrete variable
#' attach(iris)
#' plot(Petal.Length, Sepal.Length, pch=19, col=hue_map(Species))
#' legend(1, 8, legend=levels(Species), pch=19, col=hue_colors(n=nlevels(Species)))
#'
#' # Try on the elevation map of the Maunga Whau volcano
#' x <- 10*(1:nrow(volcano))
#' y <- 10*(1:ncol(volcano))
#' image(x, y, volcano, col=hue_colors(100))
#' # = typical rainbow scales bullseye effect, yuk!
#'
#' # Hue based scales may work, but with a limited range of hues
#' image(x, y, volcano, col=hue_colors(100, h=c(240,350), c=0.5))
#' persp(x, y, volcano, theta=50, phi=25, border=alpha("black", 0.3),
#'       col=hue_map(persp_facets(volcano), h=c(240,350), c=0.5))
# TODO use border=alpha("black", 0.3) everywhere
#' # Still, lightness (or chromacity)-based scales are likely to be better...
#'
#' @importFrom scales rescale
#' @export
hue_scale <- function(h=c(0,360)+40, c=0.65, l=0.65, domain=c(0,1), reverse=FALSE) {
  # check arguments
  if (length(h) != 2) {
    stop("h needs to be a vector of length 2, defining the minimum and maximum hues to use.")
  }
  
  # define the function
  f <- function(x) {
    n <- length(unique(x))
    # define colors that span a n-1 steps along the given range, to avoid cycling over the color wheel
    colors <- hcl(h=scales::rescale(x, from=domain, to=seq(h[1], h[2], length.out=n+1)[c(1,n)]), c=c, l=l)
    if (reverse) {
      colors <- rev(colors)
    }
    return(colors)
  }
  return(f)
}

#' @param ... passed to \code{\link{hue_scale}}. Note that argument \code{domain} is meaningless in functions other than \code{hue_scale} and passing it through \code{...} is an error.
#' @name hue_scale
#' @export
hue_map <- function(x, ...) {
  # force characters into factors to be able to convert them to numeric
  if (is.character(x)) {
    x <- factor(x)
  }
  # convert to numbers
  x <- as.numeric(x)
  # define the domain of the scale
  hue_scale(domain=range(x, na.rm=T), ...)(x)
}

#' @name hue_scale
#' @export
hue_palette <- function(n, ...) {
  f <- function(n) {
    hue_scale(domain=c(0,1), ...)(seq(0, 1, length.out=n))
  }
  return(f)
}

#' @name hue_scale
#' @export
hue_colors <- function(n, ...) {
  hue_palette(...)(n)
}
#' @name hue_scale
#' @export
hue.colors <- hue_colors
