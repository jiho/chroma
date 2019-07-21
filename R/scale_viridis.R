#' Viridis colors
#'
#' The \code{viridis} color palette, by Nathaniel J. Smith, Stefan van der Walt, and Eric Firing (CC0 license).
#'
#' @format A vector of length 256 containing hex values.
#' @source \url{https://bids.github.io/colormap/} for the concept and \url{https://github.com/BIDS/colormap/blob/master/colormaps.py} for the data.
#' @family mpl palettes
"viridis"


#' Viridis color scale and palette
#'
#' The \code{viridis} color palette, by Nathaniel J. Smith, Stefan van der Walt, and Eric Firing (CC0 license).
#'
#' @inheritParams interp_scale
#'
#' @template return_scales
#'
#' @export
#'
#' @family color scales and palettes
#'
#' @seealso \code{\link{viridis}} for the colors in the palette.
#'
#' @examples
#' # Get a few colors along the palette
#' show_col(
#'   viridis_palette()(20),
#'   viridis_colors(50),
#'   viridis_colors(20, reverse=TRUE)
#' )
#'
#' # 1/ Represent a continuous variable
#'
#' # Map the elevation of the Maunga Whau volcano
#' image(maunga, col=viridis_colors(100), asp=1)
#' contour(maunga, col=alpha("white", 0.5), add=TRUE)
#'
#' persp(maunga, theta=50, phi=25, scale=FALSE, expand=2,
#'       border=alpha("black", 0.4),
#'       col=viridis_map(persp_facets(maunga$z)))
#'
#' \dontrun{
#' # with ggplot2
#' library("ggplot2")
#' ggplot(maungaxyz) + coord_fixed() +
#'   geom_raster(aes(x=x, y=y, fill=z)) +
#'   geom_contour(aes(x=x, y=y, z=z), color="white", alpha=0.5) +
#'   scale_fill_viridis()
#'
#' # in spinning 3D
#' library("rgl")
#' persp3d(maunga, aspect=c(1,0.7,0.2), axes=FALSE, box=FALSE,
#'         col=viridis_map(maunga$z))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)}
#'
#' # Represent a third variable on a scatterplot
#' attach(airquality)
#' # define a scale encompassing the whole data
#' my_scale <- viridis_scale(domain=c(0,200))
#' # use the same scale for the plot and the legend
#' pars <- sidemargin()
#' plot(Wind, Temp, col=my_scale(Ozone), pch=19)
#' sidelegend(legend=c(pretty(Ozone), "NA"),
#'            col=my_scale(c(pretty(Ozone), NA)), pch=19)
#' par(pars)
#'
#' \dontrun{
#' # or with ggplot2
#' # but the light yellows at the top of the scale are difficult to see
#' # on points; either outline them or put them on a dark background
#' ggplot(airquality) +
#'   geom_point(aes(x=Wind, y=Temp, fill=Ozone), shape=21, size=2) +
#'   scale_fill_viridis()
#' ggplot(airquality) + theme_dark() +
#'   geom_point(aes(x=Wind, y=Temp, color=Ozone)) +
#'   scale_color_viridis(na.value="grey60")}
#'
#'
#' # 2/ Represent a discrete variable
#' # albeit only with a limited number of levels
#'
#' attach(iris)
#' pars <- sidemargin()
#' plot(Petal.Length, Petal.Width, pch=21, bg=viridis_map(Species))
#' sidelegend(legend=levels(Species),
#'            pt.bg=viridis_colors(n=nlevels(Species)), pch=21)
#' par(pars)
#'
#' \dontrun{
#' # or with ggplot2
#' ggplot(iris) +
#'   geom_point(aes(Petal.Length, Petal.Width, fill=Species), shape=21) +
#'   scale_fill_viridis_d()}
viridis_scale <- function(domain=c(0,1), reverse=FALSE, na.value=NULL, extrapolate=FALSE) {
  # get everything into numbers
  domain <- as.num(domain)
  if (reverse) { domain <- rev(domain)}
  f <- function(x) {
    x <- as.num(x)
    # compute colors
    xs <- rescale(x, from=domain, to=c(0,1))
    colors <- scales::colour_ramp(chroma::viridis)(xs)
    return(post_process_scale(colors, viridis_na(na.value), extrapolate, x, domain))
  }
  return(f)
}

#' @param ... passed to \code{\link{viridis_scale}} from other \code{viridis_*} functions; passed to \code{ggplot2::\link[ggplot2]{continuous_scale}} or \code{ggplot2::\link[ggplot2]{discrete_scale}} from the \code{scale_*} functions, as appropriate. NB: in all situations, passing \code{domain} is meaningless and yields an error.
#' @rdname viridis_scale
#' @export
viridis_map <- function(x, ...) { as_map(viridis_scale, x,  ...) }

#' @rdname viridis_scale
#' @export
viridis_palette <- function(...) { as_palette(viridis_scale, ...) }

#' @param n number of colors to extract from the color palette.
#' @rdname viridis_scale
#' @export
viridis_colors <- function(n, ...) { viridis_palette(...)(n) }

# Pick and appropriate NA value for a viridis scale
viridis_na <- function(na.value) {
  if (is.null(na.value)) {
    na.value <- desaturate(average(viridis_colors(50)), 10)
    # = grey of luminance equal to the average color of the scale
  }
  return(na.value)
}


## ggplot2 ----

#' @rdname viridis_scale
#' @export
scale_color_viridis_c <- function(..., reverse=FALSE, na.value=NULL, guide="colorbar") {
  cols <- if(reverse) rev(chroma::viridis) else chroma::viridis
  ggplot2::continuous_scale("colour", "viridis",
    scales::colour_ramp(cols),
    na.value=viridis_na(na.value), guide=guide, ...
  )
}
#' @rdname viridis_scale
#' @export
#' @usage NULL
scale_colour_viridis_c <- scale_color_viridis_c

#' @rdname viridis_scale
#' @export
scale_fill_viridis_c <- function(..., reverse=FALSE, na.value=NULL, guide="colorbar") {
  cols <- if(reverse) rev(chroma::viridis) else chroma::viridis
  ggplot2::continuous_scale("fill", "viridis",
    scales::colour_ramp(cols),
    na.value=viridis_na(na.value), guide=guide, ...
  )
}

#' @rdname viridis_scale
#' @export
scale_color_viridis_d <- function(..., reverse=FALSE, na.value=NULL, guide="legend") {
  cols <- if(reverse) rev(chroma::viridis) else chroma::viridis
  ggplot2::discrete_scale("colour", "viridis",
    function(n) {scales::colour_ramp(cols)(seq(0,1,length.out=n))},
    na.value=na.value, ...
  )
}
#' @rdname viridis_scale
#' @export
#' @usage NULL
scale_colour_viridis_d <- scale_color_viridis_d

#' @rdname viridis_scale
#' @export
scale_fill_viridis_d <- function(..., reverse=FALSE, na.value=NULL, guide="legend") {
  cols <- if(reverse) rev(chroma::viridis) else chroma::viridis
  ggplot2::discrete_scale("fill", "viridis",
    function(n) {scales::colour_ramp(cols)(seq(0,1,length.out=n))},
    na.value=na.value, ...
  )
}

# Make the continuous versions the default because it is the most common use case
#' @rdname viridis_scale
#' @export
#' @usage NULL
scale_fill_viridis <- scale_fill_viridis_c
#' @rdname viridis_scale
#' @export
#' @usage NULL
scale_color_viridis <- scale_color_viridis_c
#' @rdname viridis_scale
#' @export
#' @usage NULL
scale_colour_viridis <- scale_color_viridis_c


