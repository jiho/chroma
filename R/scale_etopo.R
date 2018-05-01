#' Colors from ETOPO1
#'
#' @format A data.frame with 43 rows and 2 variables:
#' \describe{
#'   \item{altitude}{the altitude.}
#'   \item{color}{the hex code of the correpsonding color.}
#' }
#' @source \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/ngdc/tn/ETOPO1.png.index.html}
#' @seealso \code{\link{etopo_scale}} to use the palette.
"etopo"


#' ETOPO1 scale and palette
#'
#' Color scale and palette inspired from the representation of the ETOPO1 global relief model. These scales are special because the mapping between colors and values is predefined to work for altitudes (or depths when the numbers are negative).
#'
#' @inheritParams interp_scale
#' @param exact.until integer, when more than \code{exact.until} colors need to be computed, a fast but not exact alternative algorithm is used. This number is increased compared to the default in \code{\link{interp_scale}} because some transitions in color along the ETOPO1 palette are sharp, in particular around altitude=0. To get exact interpolation all the time, use a very large number.
#' @param ... passed to \code{\link{etopo_scale}} or to \code{scale_*_gradientn} for the \code{scale_*_etopo} functions.
#'
#' @template return_scales
#'
#' @family color scales and palettes
#'
#' @references The topographic colors are based on GMT globe by Lester M. Anderson of CASP, UK, modified by Jesse Varner and Elliot Lim (NOAA/NGDC) to have a smaller band of white at the highest elevations. The bathymetry is based on GMT haxby, popularised by Bill Haxby, LDEO. See \url{https://www.ngdc.noaa.gov/mgg/global/global.html}.
#' @seealso \code{\link{etopo}} for the colors in the palette and the associated altitudes.
#'
#' @export
#'
#' @examples
#' # Defining a scale in that case has little value since it cannot be
#' # customized. Explore the default mapping
#' altitudes <- c(-5000, -1000, -10, 0, 10, 200, 500, 5000)
#' show_col(etopo_map(altitudes))
#' # Explore the full range of colors
#' show_col(etopo_map(seq(-8000, 8000, length=500)))
#'
#' # It is possible to get n colors from the palette but they loose their
#' # association with specific altitudes
#' etopo_colors(5)
#'
#' # The goal of these scales is to color maps and elevation models. Here is
#' # one centered on Thailand, showcasing high mountains and deep trenches
#' levs <- seq(-9000,6000,by=500)
#' contour(thai, levels=levs, col=etopo_map(levs), asp=1.03)
#'
#' filled.contour(thai, levels=levs, col=etopo_map(levs), asp=1.03)
#'
#' persp(thai, theta=30, phi=25, border=alpha("black", 0.2), expand=0.2,
#'       col=etopo_map(persp_facets(thai$z)))
#'
#' \dontrun{
#' # 3D rotating map
#' library("rgl")
#' # To get a perfect land/sea mapping, the colors need to be computed
#' # exactly based on the input altitude values; set `exact.until` very high
#' # do achieve this (but this makes the function slower of course).
#' persp3d(thai, aspect=c(1,0.96,0.2), axes=FALSE, box=FALSE,
#'         col=etopo_map(thai$z, exact.until=10^5))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#'
#' # ggplot2 maps
#' library("ggplot2")
#' ggplot(thaixyz) + coord_quickmap() +
#'   geom_contour(aes(x, y, z=z, color=..level..), breaks=levs) +
#'   theme_light() + scale_color_etopo() +
#'   scale_xy_map()
#' ggplot(thaixyz) + coord_quickmap() +
#'   geom_raster(aes(x, y, fill=z)) +
#'   scale_fill_etopo() +
#'   geom_contour(aes(x, y, z=z), breaks=0, color="black", size=1) +
#'   scale_xy_map()
#' }
etopo_scale <- function(exact.until=1000) {
  bar <- interp_scale(colors=chroma::etopo$color, model="lab", interp="linear", values=chroma::etopo$altitude, exact.until=exact.until)
}

#' @rdname etopo_scale
#' @export
etopo_map <- function(x, ...) {
  etopo_scale(...)(x)
}

#' @rdname etopo_scale
#' @export
etopo_palette <- function(exact.until=1000, ...) {
  interp_palette(colors=chroma::etopo$color, model="lab", interp="linear", exact.until=exact.until, ...)
}

#' @rdname etopo_scale
#' @export
etopo_colors <- function(n, ...) {
  etopo_palette(...)(n=n)
}

#' @rdname etopo_scale
#' @export
scale_fill_etopo <- function(...) {
  ggplot2::scale_fill_gradientn(..., colors=chroma::etopo$color, values=scales::rescale(chroma::etopo$altitude), limits=range(chroma::etopo$altitude))
}
#' @rdname etopo_scale
#' @export
scale_color_etopo <- function(...) {
  ggplot2::scale_color_gradientn(..., colors=chroma::etopo$color, values=scales::rescale(chroma::etopo$altitude), limits=range(chroma::etopo$altitude))
}
#' @rdname etopo_scale
#' @export
#' @usage NULL
scale_colour_etopo <- scale_color_etopo
