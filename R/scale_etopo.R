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
#' levs <- seq(-9000, 6000, by=500)
#' contour(thai, levels=levs, col=etopo_map(levs), asp=1.03)
#'
#' filled.contour(thai, levels=levs, col=etopo_map(levs), asp=1.03)
#'
#' persp(thai, theta=30, phi=25, border=alpha("black", 0.2), expand=0.2,
#'       col=etopo_map(persp_facets(thai$z)))
#'
#' \dontrun{
#' # in spinning 3D
#' library("rgl")
#' persp3d(thai, aspect=c(1,0.96,0.2), axes=FALSE, box=FALSE,
#'         col=etopo_map(thai$z))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#'
#' # or with ggplot2
#' library("ggplot2")
#' ggplot(thaixyz) + coord_quickmap() +
#'   geom_contour(aes(x, y, z=z, color=..level..), breaks=levs) +
#'   theme_light() + scale_color_etopo() + scale_xy_map()
#' ggplot(thaixyz) + coord_quickmap() +
#'   geom_raster(aes(x, y, fill=z)) +
#'   scale_fill_etopo() + scale_xy_map() +
#'   geom_contour(aes(x, y, z=z), breaks=0, color="black", size=1) }
etopo_scale <- function(na.value=NULL, ...) {
  function(x) {
    colors <- scales::gradient_n_pal(colours=chroma::etopo$color, values=chroma::etopo$altitude)(x)
    na_replace(colors, etopo_na(na.value))
  }
}

#' @rdname etopo_scale
#' @export
etopo_map <- function(x, ...) { etopo_scale(...)(x) }

#' @rdname etopo_scale
#' @export
etopo_palette <- function(...) { as_palette(etopo_scale, ...) }

#' @rdname etopo_scale
#' @export
etopo_colors <- function(n, ...) { etopo_palette(...)(n) }

etopo_na <- function(na.value) {
  if (is.null(na.value)) {
    na.value <- "#8A8A8A"
    # = grey of average luminance compared to the other colours on the scale
  }
  return(na.value)
}

#' @rdname etopo_scale
#' @export
scale_fill_etopo <- function(na.value=NULL, ...) {
  ggplot2::scale_fill_gradientn(..., colors=chroma::etopo$color, values=scales::rescale(chroma::etopo$altitude), limits=range(chroma::etopo$altitude), na.value=etopo_na(na.value))
}
#' @rdname etopo_scale
#' @export
scale_color_etopo <- function(na.value=NULL, ...) {
  ggplot2::scale_color_gradientn(..., colors=chroma::etopo$color, values=scales::rescale(chroma::etopo$altitude), limits=range(chroma::etopo$altitude), na.value=etopo_na(na.value))
}
#' @rdname etopo_scale
#' @export
#' @usage NULL
scale_colour_etopo <- scale_color_etopo
