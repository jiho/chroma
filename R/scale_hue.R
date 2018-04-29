#' Hue scale and palette
#'
#' Hue-based color scale, in HCL space.
#'
#' @param h range of hues to use, a vector of length 2 with either angles around the color wheel, in \code{[0, 360]} (angles outside of the range are rotated back to within \code{[0, 360]}: 380 = 20, -15 = 345, etc.), or colors (hex or named) from which the hue is extracted (by function \code{\link{hue}}).
#' @template param_hue
#' @template param_chromacity
#' @template param_lightness
#' @inheritParams interp_scale
#' @param ... passed to \code{\link{hue_scale}}. Note that argument \code{domain} is meaningless in functions other than \code{hue_scale} and passing it through \code{...} is an error.
#' @param full.circle when the range of hues specified in \code{h} spans 360°, this argument determines if the color scale should also span the full circle (which results in the same color being associated with different values, at the extremes of the scale), or not. By default it is set to FALSE to avoid this caveat. When \code{h} does not span 360°, this argument is ignored because the caveat disappears.
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
#' image(maunga, col=hue_colors(100))
#' # = typical rainbow scales bullseye effect, yuk!
#'
#' # Hue based scales may work, but with a limited range of hues
#' image(maunga, col=hue_colors(100, h=c(240,350), c=0.5))
#' contour(maunga, col=alpha("white", 0.5), add=TRUE)
#'
#' persp(maunga, theta=50, phi=25, border=alpha("black", 0.3),
#'       col=hue_map(persp_facets(maunga$z), h=c(240,350), c=0.5))
#' # Still, lightness (or chromacity)-based scales are likely to be better...
#'
hue_scale <- function(h=c(0,360)+40, c=0.65, l=0.65, domain=c(0,1), reverse=FALSE, full.circle=FALSE, na.value=NULL) {
  # check arguments
  if (length(h) != 2) {
    stop("h must be a vector of length 2, defining the range of hues to use.")
  }

  # allow to specify hues as colors rather than angles
  h <- hue(h, modulo=FALSE)

  # verify the range of hues
  hue_range <- diff(h)
  if (abs(hue_range) > 360) {
    h2 <- h[1] + 360*sign(hue_range)
    warning(paste0("The range of hues chosen is wider than 360\u00B0 (",h[1]," -> ",h[2]," = ", hue_range,"\u00B0).\n  Several values would be mapped to the same hue, which is probably not desirable.\n  The range was clipped to ",h[1]," -> ",h2," = 360\u00B0."))
    h[2] <- h2
    hue_range <- 360
  }

  # change the direction along the color wheel
  if (reverse) {
    h <- rev(h)
  }

  # define the function
  f <- function(x) {
    # force characters into factors to be able convert to numbers
    if (is.character(x)) { x <- factor(x) }
    # convert to number
    x <- as.numeric(x)

    # expand the domain to avoid cycling around the color grid
    if (hue_range == 360 & !full.circle) {
      n <- length(unique(x))
      domain <- domain * c(1, (n+1)/n)
    }

    # map colors
    colors <- hcl(h=scales::rescale(x, from=domain, to=h), c=c, l=l)
    }
    return(colors)
  }
  return(f)
}

#' @rdname hue_scale
#' @export
hue_map <- function(x, ...) {
  # force characters into factors to be able to convert them to numeric
  if (is.character(x)) { x <- factor(x) }
  # convert to numbers
  x <- as.numeric(x)
  # define the domain of the scale
  hue_scale(domain=range(x, na.rm=TRUE), ...)(x)
}

#' @rdname hue_scale
#' @export
hue_palette <- function(n, ...) {
  f <- function(n) {
    hue_scale(domain=c(1,n), ...)(1:n)
  }
  return(f)
}

#' @rdname hue_scale
#' @export
hue_colors <- function(n, ...) {
  hue_palette(...)(n)
}
