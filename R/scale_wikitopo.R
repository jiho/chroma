#' Colors from Wikipedia topographic conventions
#'
#' @format A data.frame with 29 rows and 2 variables:
#' \describe{
#'   \item{altitude}{the altitude.}
#'   \item{color}{the hex code of the correpsonding color.}
#' }
#' @source \url{https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps}
#' @seealso \code{\link{wikitopo_scale}} to use the palette.
"wikitopo"


#' Wikipedia topographic scale and palette
#'
#' Color scale and palette from Wikipedia's topographic maps. These scales are special because the mapping between colors and values is predefined to work for altitudes (or depths when the numbers are negative).
#'
#' @inheritParams interp_scale
#' @param exact.until integer, when more than \code{exact.until} colors need to be computed, a fast but not exact alternative algorithm is used. This number is increased compared to the default in \code{\link{interp_scale}} because some transitions in color along the wikipedia topographic palette are sharp, in particular around altitude=0. To get exact interpolation all the time, use a very large number.
#' @param ... passed to \code{\link{wikitopo_scale}} or to \code{scale_*_gradientn} for the \code{scale_*_wikitopo} functions.
#'
#' @template return_scales
#'
#' @family color scales and palettes
#'
#' @references \url{https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps}.
#'
#' @examples
#' # Defining a scale in that case has little value since it cannot be
#' # customized. Explore the default mapping
#' altitudes <- c(-5000, -1000, -10, 0, 10, 200, 500, 5000)
#' show_col(wikitopo_map(altitudes))
#' # Explore the full range of colors
#' show_col(wikitopo_map(seq(-8000, 8000, length=500)))
#'
#' # It is possible to get n colors from the palette but they loose their
#' # association with specific altitudes
#' wikitopo_colors(5)
#'
#' # The goal of these scales is to color maps and elevation models. Here is
#' # one centered on Thailand, showcasing high mountains and deep trenches
#' levs <- seq(-9000, 6000, by=500)
#' contour(thai, levels=levs, col=wikitopo_map(levs), asp=1.03)
#'
#' filled.contour(thai, levels=levs, col=wikitopo_map(levs), asp=1.03)
#'
#' persp(thai, theta=30, phi=25, border=alpha("black", 0.2), expand=0.2,
#'       col=wikitopo_map(persp_facets(thai$z)))
#'
#' \dontrun{
#' # 3D rotating map
#' library("rgl")
#' # To get a perfect land/sea mapping, the colors need to be computed
#' # exactly based on the input altitude values; set `exact.until` very high
#' # do achieve this (but this makes the function slower of course).
#' persp3d(thai, aspect=c(1,0.96,0.2), axes=FALSE, box=FALSE,
#'         col=wikitopo_map(thai$z, exact.until=10))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#'
#' # ggplot2 maps
#' library("ggplot2")
#' ggplot(thaixyz) + coord_quickmap() +
#'   geom_contour(aes(x, y, z=z, colour=..level..), breaks=levs) +
#'   geom_contour(aes(x, y, z=z), breaks=0, colour="black", linetype="11") +
#'   theme_light() + scale_colour_wikitopo() +
#'   scale_xy_map()
#' ggplot(thaixyz) + coord_quickmap() +
#'   geom_raster(aes(x, y, fill=z)) +
#'   scale_fill_wikitopo() +
#'   geom_contour(aes(x, y, z=z), breaks=0, colour="black", alpha=0.5) +
#'   scale_xy_map()
#' }
#' @export
wikitopo_scale <- function(exact.until=1000) {
  bar <- interp_scale(colors=chroma::wikitopo$color, model="lab", interp="linear", values=chroma::wikitopo$altitude, exact.until=exact.until)
}

#' @rdname wikitopo_scale
#' @export
wikitopo_map <- function(x, ...) {
  wikitopo_scale(...)(x)
}

#' @rdname wikitopo_scale
#' @export
wikitopo_palette <- function(exact.until=1000, ...) {
  interp_palette(colors=chroma::wikitopo$color, model="lab", interp="linear", exact.until=exact.until, ...)
}

#' @rdname wikitopo_scale
#' @export
wikitopo_colors <- function(n, ...) {
  wikitopo_palette(...)(n=n)
}

#' @rdname wikitopo_scale
#' @export
scale_fill_wikitopo <- function(...) {
  ggplot2::scale_fill_gradientn(..., colors=chroma::wikitopo$color, values=scales::rescale(chroma::wikitopo$altitude), limits=range(chroma::wikitopo$altitude))
}
#' @rdname wikitopo_scale
#' @export
scale_color_wikitopo <- function(...) {
  ggplot2::scale_colour_gradientn(..., colors=chroma::wikitopo$color, values=scales::rescale(chroma::wikitopo$altitude), limits=range(chroma::wikitopo$altitude))
}
#' @rdname wikitopo_scale
#' @export
#' @usage NULL
scale_colour_wikitopo <- scale_color_wikitopo

