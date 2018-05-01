#' Lightness scale and palette
#'
#' Lightness-based color scale, in HCL space.
#'
#' @param l lightness, vector of two numbers in \code{[0,1]} (0 is darkest, 1 is brightest) giving the minimum and maximum lightness along the scale.
#' @inheritParams interp_scale
#' @inheritParams hcl
#' @param ... passed to \code{\link{light_scale}} from other \code{light_*} functions; passed to \code{ggplot2::\link[ggplot2]{continuous_scale}} from the \code{scale_*} functions. NB: in all situations, passing \code{domain} is meaningless and yields an error.
#'
#' @template details_hcl
#'
#' @template return_scales
#'
#' @template seealso_hcl_scales
#' @seealso \code{\link{luminance}} for a the computation of perceived luminance and the creation of perception-based luminance palettes.
#' @family color scales and palettes
#'
#' @export
#'
#' @examples
#' # Define a dark-to-light blue scale
#' blues <- light_scale(h=220)
#' # and apply it to some data
#' blues(x=c(0, 0.2, 0.6, 1))
#'
#' # Define a palette function
#' blues_pal <- light_palette(h=220, c=0.3)
#' # and get 10 colors from it
#' blues_pal(n=10)
#' show_col(blues_pal(n=10))
#' # or use the shortcut
#' show_col(light_colors(n=50, h=220, c=0.3))
#'
#' # Determine hue from a color and then define a lightness scale
#' blues <- light_colors(n=50, h="dodgerblue")
#' greens <- light_colors(n=50, h="green")
#' golds <- light_colors(n=50, h="gold")
#' pinks <- light_colors(n=50, h="deeppink")
#' show_col(blues, greens, golds, pinks)
#'
#' # Perceived lightness (computed by luminance()) increases in a similar
#' # way across the four hues, making the palettes almost comparable.
#' # (This would not be the case with a HSL or HSV gradient)
#' plot(  luminance(blues),  col=blues[40])
#' points(luminance(greens), col=greens[40])
#' points(luminance(golds),  col=golds[40])
#' points(luminance(pinks),  col=pinks[40])
#'
#' # Lightness scales are good for continuous variables
#' # such as the elevation of the Maunga Whau volcano
#' image(maunga, col=light_colors(100, h=140))
#' contour(maunga, col=alpha("white", 0.5), add=TRUE)
#'
#' filled.contour(maunga, color.palette=light_palette(h=140))
#'
#' persp(maunga, theta=50, phi=25, scale=FALSE, expand=2,
#'       border=alpha("black", 0.4),
#'       col=light_map(persp_facets(maunga$z), h=140))
#'
#' \dontrun{
#' # in spining 3D
#' library("rgl")
#' persp3d(maunga, aspect=c(1,0.7,0.2), axes=FALSE, box=FALSE,
#'         col=light_map(maunga$z, h=140))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#'
#' # and with ggplot2
#' library("ggplot2")
#' ggplot(maungaxyz) + coord_fixed() +
#'   geom_raster(aes(x=x, y=y, fill=z)) +
#'   geom_contour(aes(x=x, y=y, z=z), color="white", alpha=0.5) +
#'   scale_fill_light(h=140)
#' }
#'
#' # Or they could be used to map a third variable on a scatterplot
#' attach(airquality)
#' # define a scale encompassing the whole data
#' blue_scale <- light_scale(h="cornflowerblue", domain=c(0,200))
#' # use it on a plot and in the legend
#' pars <- sidemargin()
#' plot(Wind, Temp, col=blue_scale(Ozone), pch=19)
#' sidelegend(legend=c(pretty(Ozone), "NA"),
#'            col=blue_scale(c(pretty(Ozone), NA)), pch=19)
#' par(pars)
#'
#' # Note that the missing value lightess matches the rest of the scale
#' plot(Wind, Temp, col=light_map(Ozone, l=c(0.5, 0.9)), pch=19)
#' plot(Wind, Temp, col=light_map(Ozone, l=c(0.1, 0.5)), pch=19)
#'
#'
#' # They are not really appropriate for categorical variables though
#' attach(iris)
#' plot(Petal.Length, Petal.Width, col=chroma_map(Species), pch=19)
#' legend(1, 2, legend=levels(Species),
#'              col=light_colors(n=nlevels(Species)), pch=19)
#' # a hue-based scale would be much better (see ?hue_scale)
light_scale <- function(l=c(0.1,0.9), c=0.5, h=0, domain=c(0,1), reverse=FALSE, na.value=NULL, extrapolate=FALSE) {
  # check arguments
  if (length(l) != 2) {
    stop("l needs to be a vector of length 2, defining the minimum and maximum lightness to use.")
  }

  # change the direction of the scale
  if (reverse) { l <- rev(l) }

  # if the na.value is not defined, pick a good default
  na.value <- light_na(na.value, l=l)

  # define the function
  f <- function(x) {
    s <- as.num(x)
    domain <- as.num(domain)
    colors <- hcl(h=h, c=c, l=rescale(x, from=domain, to=l))
    return(post_process_scale(colors, na.value, extrapolate, x, domain))
  }
  return(f)
}

#' @rdname light_scale
#' @export
light_map <- function(x, ...) { as_map(light_scale, x, ...) }

#' @rdname light_scale
#' @export
light_palette <- function(...) { as_palette(light_scale, ...) }

#' @rdname light_scale
#' @export
light_colors <- function(n, ...) { light_palette(...)(n) }


# Pick a good missing value color for a hue scale
# when not defined (NULL), pick a grey of average lightness along the scale
light_na <- function(na.value, l) {
  if (is.null(na.value)) {
    na.value <- hcl(h=0, c=0, l=mean(l))
  }
  return(na.value)
}

## ggplot ----

#' @rdname light_scale
#' @export
scale_color_light <- function(..., l=c(0.1,0.9), c=0.5, h=0, reverse=FALSE, na.value=NULL, guide="colorbar") {
  ggplot2::continuous_scale("colour", "light",
    light_scale(l=l, c=c, h=h, reverse=reverse),
    na.value=light_na(na.value, l=l), guide=guide, ...
  )
}
#' @rdname light_scale
#' @export
#' @usage NULL
scale_colour_light <- scale_color_light

#' @rdname light_scale
#' @export
scale_fill_light <- function(..., l=c(0.1,0.9), c=0.5, h=0, reverse=FALSE, na.value=NULL, guide="colorbar") {
  ggplot2::continuous_scale("fill", "light",
    light_scale(l=l, c=c, h=h, reverse=reverse),
    na.value=light_na(na.value, l=l), guide=guide, ...
  )
}

# NB: discrete light scales do not make much sense so we do not define any.
