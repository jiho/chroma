#' Hue scale and palette
#'
#' Hue-based color scale, in HCL space.
#'
#' @param h range of hues to use, a vector of length 2 with either angles around the color wheel, in \code{[0,360]} (angles outside of the range are rotated back to within \code{[0,360]}: 380 = 20, -15 = 345, etc.), or colors (hex or named) from which the hue is extracted (by function \code{\link{hue}}).
#' @inheritParams interp_scale
#' @inheritParams hcl
#' @param ... passed to \code{\link{hue_scale}} from other \code{hue_*} functions; passed to \code{ggplot2::\link[ggplot2]{discrete_scale}} or \code{ggplot2::\link[ggplot2]{continuous_scale}} from the \code{scale_*} functions, as appropriate. NB: in all situations, passing \code{domain} is meaningless and yields an error.
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
#' # Display the full hue range with
#' x <- 0:360
#' plot(x, rep(0,length(x)), col=hue_map(x, h=c(0,360), full.circle=TRUE), ylab="", pch="|", cex=5)
#'
#' # Define a perceptually-correct "rainbow"-like scale function
#' rainbow_scale <- hue_scale()
#' # and apply it to some data
#' show_col(rainbow_scale(x=c(0, 0.2, 0.6, 1)))
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
#' # are appropriate to distinguish among levels of a qualitative variable
#' attach(iris)
#' plot(Petal.Length, Petal.Width, col=hue_map(Species), pch=19)
#' legend(1, 2, legend=levels(Species), col=hue_colors(n=nlevels(Species)), pch=19)
#'
#' # Let us try with a quantitative variable
#' image(maunga, col=hue_colors(100))
#' # = typical rainbow scale bullseye effect, yuk!
#' # but, with a limited hue range, they can be OK
#' image(maunga, col=hue_colors(10, h=c(170, 90), l=0.6))
#' contour(maunga, col=alpha("white", 0.5), add=TRUE)
#'
#' persp(maunga, theta=50, phi=25, scale=FALSE, expand=2,
#'       border=alpha("black", 0.4),
#'       col=hue_map(persp_facets(maunga$z), h=c(170, 90), l=0.6))
#' # Still, lightness (or chromacity) based scales are likely to be better...
#'
#' # To create a legend for a continuous variable, we need to define the
#' # scale with its domain and then use it for both the plot and the legend.
#' attach(airquality)
#' oz_scale <- hue_scale(h=c(250,350), l=0.5, domain=range(Ozone, na.rm=TRUE))
#' plot(Wind, Temp, col=oz_scale(Ozone), pch=19)
#' legend(17, 95, legend=pretty(Ozone), col=oz_scale(pretty(Ozone)), pch=19)
#'
#' # Note how the missing value grey matches the rest of the colors on the scale
#' plot(Wind, Temp, col=hue_map(Ozone, h=c(250,350), l=0.5), pch=19)
#' plot(Wind, Temp, col=hue_map(Ozone, h=c(250,350), l=0.8), pch=19)
#' plot(Wind, Temp, col=hue_map(Ozone, h=c(250,350), l=0.3), pch=19)
#'
#' # Make the plot nicer to read by putting the legend on the side
#' pars <- sidemargin()
#' plot(Wind, Temp, col=oz_scale(Ozone), pch=19)
#' sidelegend(legend=pretty(Ozone), col=oz_scale(pretty(Ozone)), pch=19)
#' par(pars)
#'
#' \dontrun{
#' # or just use ggplot2
#' library("ggplot2")
#' ggplot(iris) +
#'   geom_point(aes(x=Petal.Length, y=Petal.Width, color=Species)) +
#'   scale_color_hue_d()
#' ggplot(airquality) +
#'   geom_point(aes(x=Wind, y=Temp, color=Ozone)) +
#'   scale_color_hue_c()}
hue_scale <- function(h=c(0,360)+40, c=0.65, l=0.65, domain=c(0,1), reverse=FALSE, full.circle=FALSE, na.value=NULL, extrapolate=FALSE) {
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
  if (reverse) { h <- rev(h) }

  # if the na.value is not defined, pick a good default
  na.value <- hue_na(na.value, l=l)

  # define the function
  f <- function(x) {
    x <- as.num(x)
    domain <- as.num(domain)

    # expand the domain to avoid cycling around the color grid
    if (hue_range == 360 & !full.circle) {
      n <- length(unique(x))
      domain <- domain * c(1, (n+1)/n)
    }

    colors <- hcl(h=rescale(x, from=domain, to=h), c=c, l=l)
    return(post_process_scale(colors, na.value, extrapolate, x, domain))
  }
  return(f)
}

#' @rdname hue_scale
#' @export
hue_map <- function(x, ...) { as_map(hue_scale, x, ...) }

#' @rdname hue_scale
#' @export
hue_palette <- function(...) { as_palette(hue_scale, ...) }

#' @rdname hue_scale
#' @export
hue_colors <- function(n, ...) { hue_palette(...)(n) }

# Pick a good missing value color for a hue scale
# when not defined (NULL), pick a grey with the same luminance as the other colors in the scale
hue_na <- function(na.value, l) {
  if (is.null(na.value)) {
    na.value <- hcl(h=0, c=0, l=l)
  }
  return(na.value)
}

## ggplot ----

#' @rdname hue_scale
#' @export
scale_color_hue_d <- function(..., h=c(0,360)+40, c=0.65, l=0.65, reverse=FALSE, full.circle=FALSE, na.value=NULL) {
  ggplot2::discrete_scale("colour", "hue",
    hue_palette(h=h, c=c, l=l, reverse=reverse, full.circle=full.circle),
    na.value=hue_na(na.value, l=l), ...
  )
}
#' @rdname hue_scale
#' @export
#' @usage NULL
scale_colour_hue_d <- scale_color_hue_d

#' @rdname hue_scale
#' @export
scale_fill_hue_d <- function(..., h=c(0,360)+40, c=0.65, l=0.65, reverse=FALSE, full.circle=FALSE, na.value=NULL) {
  ggplot2::discrete_scale("fill", "hue",
    hue_palette(h=h, c=c, l=l, reverse=reverse, full.circle=full.circle),
    na.value=hue_na(na.value, l=l), ...
  )
}

#' @rdname hue_scale
#' @export
scale_color_hue_c <- function(..., h=c(250,350), c=0.65, l=0.65, reverse=FALSE, full.circle=FALSE, na.value=NULL, guide="colourbar") {
  ggplot2::continuous_scale("colour", "hue",
    hue_scale(h=h, c=c, l=l, reverse=reverse, full.circle=full.circle),
    na.value=hue_na(na.value, l=l), guide=guide, ...
  )
}
#' @rdname hue_scale
#' @export
#' @usage NULL
scale_colour_hue_c <- scale_color_hue_c

#' @rdname hue_scale
#' @export
scale_fill_hue_c <- function(..., h=c(250,350), c=0.65, l=0.65, reverse=FALSE, full.circle=FALSE, na.value=NULL, guide="colourbar") {
  ggplot2::continuous_scale("fill", "hue",
    hue_scale(h=h, c=c, l=l, reverse=reverse, full.circle=full.circle),
    na.value=hue_na(na.value, l=l), guide=guide, ...
  )
}
