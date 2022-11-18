#' Turbo colors
#'
#' The \code{turbo} color palette, by Anton Mikhailov (Apache-2.0 license).
#'
#' @format A vector of length 256 containing hex values.
#' @source \url{https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html}
#' @family palettes
"turbo"


#' Turbo color scale and palette
#'
#' The \code{turbo} color palette, by Anton Mikhailov (Apache-2.0 license).
#'
#' @inheritParams interp_scale
#'
#' @template return_scales
#'
#' @export
#'
#' @family color scales and palettes
#'
#' @seealso \code{\link{turbo}} for the colors in the palette.
#'
#' @examples
#' # Get a few colors along the palette
#' show_col(
#'   turbo_palette()(20),
#'   turbo_colors(50),
#'   turbo_colors(20, reverse=TRUE)
#' )
#'
#' # 1/ Represent a continuous variable
#'
#' # Map the elevation of the Maunga Whau volcano
#' image(maunga, col=turbo_colors(100), asp=1)
#' contour(maunga, col=alpha("white", 0.5), add=TRUE)
#'
#' persp(maunga, theta=50, phi=25, scale=FALSE, expand=2,
#'       border=alpha("black", 0.4),
#'       col=turbo_map(persp_facets(maunga$z)))
#'
#' \dontrun{
#' # with ggplot2
#' library("ggplot2")
#' ggplot(maungaxyz) + coord_fixed() +
#'   geom_raster(aes(x=x, y=y, fill=z)) +
#'   geom_contour(aes(x=x, y=y, z=z), color="white", alpha=0.5) +
#'   scale_fill_turbo()
#'
#' # in spinning 3D
#' library("rgl")
#' persp3d(maunga, aspect=c(1,0.7,0.2), axes=FALSE, box=FALSE,
#'         col=turbo_map(maunga$z))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#' }
#'
#' # Represent a third variable on a scatterplot
#' attach(airquality)
#' # define a scale encompassing the whole data
#' my_scale <- turbo_scale(domain=c(0,200))
#' # use the same scale for the plot and the legend
#' pars <- sidemargin()
#' plot(Wind, Temp, col=my_scale(Ozone), pch=19)
#' sidelegend(legend=c(pretty(Ozone), "NA"),
#'            col=my_scale(c(pretty(Ozone), NA)), pch=19)
#' par(pars)
#'
#' \dontrun{
#' # or with ggplot2
#' ggplot(airquality) +
#'   geom_point(aes(x=Wind, y=Temp, fill=Ozone), shape=21, size=2) +
#'   scale_fill_turbo()
#'}
turbo_scale <- function(domain=c(0,1), reverse=FALSE, na.value=NULL, extrapolate=FALSE) {
  # get everything into numbers
  domain <- as.num(domain)
  if (reverse) { domain <- rev(domain)}
  f <- function(x) {
    x <- as.num(x)
    # compute colors
    xs <- rescale(x, from=domain, to=c(0,1))
    colors <- scales::colour_ramp(chroma::turbo)(xs)
    return(post_process_scale(colors, turbo_na(na.value), extrapolate, x, domain))
  }
  return(f)
}

#' @param ... passed to \code{\link{turbo_scale}} from other \code{turbo_*} functions; passed to \code{ggplot2::\link[ggplot2]{continuous_scale}} or \code{ggplot2::\link[ggplot2]{discrete_scale}} from the \code{scale_*} functions, as appropriate. NB: in all situations, passing \code{domain} is meaningless and yields an error.
#' @rdname turbo_scale
#' @export
turbo_map <- function(x, ...) { as_map(turbo_scale, x,  ...) }

#' @rdname turbo_scale
#' @export
turbo_palette <- function(...) { as_palette(turbo_scale, ...) }

#' @param n number of colors to extract from the color palette.
#' @rdname turbo_scale
#' @export
turbo_colors <- function(n, ...) { turbo_palette(...)(n) }

# Pick and appropriate NA value for a turbo scale
turbo_na <- function(na.value) {
  if (is.null(na.value)) {
    na.value <- desaturate(chroma::turbo[128], 10)
    # = grey corresponding to the middle color of the scale
  }
  return(na.value)
}


## ggplot2 ----

#' @rdname turbo_scale
#' @export
scale_color_turbo_c <- function(..., reverse=FALSE, na.value=NULL, guide="colorbar") {
  cols <- if(reverse) rev(chroma::turbo) else chroma::turbo
  ggplot2::continuous_scale("colour", "turbo",
    scales::colour_ramp(cols),
    na.value=turbo_na(na.value), guide=guide, ...
  )
}
#' @rdname turbo_scale
#' @export
#' @usage NULL
scale_colour_turbo_c <- scale_color_turbo_c

#' @rdname turbo_scale
#' @export
scale_fill_turbo_c <- function(..., reverse=FALSE, na.value=NULL, guide="colorbar") {
  cols <- if(reverse) rev(chroma::turbo) else chroma::turbo
  ggplot2::continuous_scale("fill", "turbo",
    scales::colour_ramp(cols),
    na.value=turbo_na(na.value), guide=guide, ...
  )
}

#' @rdname turbo_scale
#' @export
scale_color_turbo_d <- function(..., reverse=FALSE, na.value=NULL, guide="legend") {
  cols <- if(reverse) rev(chroma::turbo) else chroma::turbo
  ggplot2::discrete_scale("colour", "turbo",
    function(n) {scales::colour_ramp(cols)(seq(0,1,length.out=n))},
    na.value=na.value, ...
  )
}
#' @rdname turbo_scale
#' @export
#' @usage NULL
scale_colour_turbo_d <- scale_color_turbo_d

#' @rdname turbo_scale
#' @export
scale_fill_turbo_d <- function(..., reverse=FALSE, na.value=NULL, guide="legend") {
  cols <- if(reverse) rev(chroma::turbo) else chroma::turbo
  ggplot2::discrete_scale("fill", "turbo",
    function(n) {scales::colour_ramp(cols)(seq(0,1,length.out=n))},
    na.value=na.value, ...
  )
}

# Make the continuous versions the default because it is the most common use case
#' @rdname turbo_scale
#' @export
#' @usage NULL
scale_fill_turbo <- scale_fill_turbo_c
#' @rdname turbo_scale
#' @export
#' @usage NULL
scale_color_turbo <- scale_color_turbo_c
#' @rdname turbo_scale
#' @export
#' @usage NULL
scale_colour_turbo <- scale_color_turbo_c


