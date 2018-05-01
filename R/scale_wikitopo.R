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
#' @param ... passed to \code{\link{wikitopo_scale}} or to \code{scale_*_gradientn} for the \code{scale_*_wikitopo} functions.
#'
#' @template return_scales
#'
#' @family color scales and palettes
#'
#' @references \url{https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps}.
#'
#' @export
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
#' # in spinning 3D
#' library("rgl")
#' persp3d(thai, aspect=c(1,0.96,0.2), axes=FALSE, box=FALSE,
#'         col=wikitopo_map(thai$z))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#'
#' # or with ggplot2
#' library("ggplot2")
#' ggplot(thaixyz) + coord_quickmap() +
#'   geom_contour(aes(x, y, z=z, color=..level..), breaks=levs) +
#'   theme_light() + scale_color_wikitopo() + scale_xy_map()
#' ggplot(thaixyz) + coord_quickmap() +
#'   geom_raster(aes(x, y, fill=z)) +
#'   scale_fill_wikitopo() + scale_xy_map() +
#'   geom_contour(aes(x, y, z=z), breaks=0, color="black") }
wikitopo_scale <- function(na.value=NULL, ...) {
  function(x) {
    colors <- scales::gradient_n_pal(colours=chroma::wikitopo$color, values=chroma::wikitopo$altitude)(x)
    na_replace(colors, wikitopo_na(na.value))
  }
}

#' @rdname wikitopo_scale
#' @export
wikitopo_map <- function(x, ...) { wikitopo_scale(...)(x) }

#' @rdname wikitopo_scale
#' @export
wikitopo_palette <- function(...) { as_palette(wikitopo_scale, ...) }

#' @rdname wikitopo_scale
#' @export
wikitopo_colors <- function(n, ...) { wikitopo_palette(...)(n) }

wikitopo_na <- function(na.value) {
  if (is.null(na.value)) {
    na.value <- "#C5C5C5"
    # = grey of average luminance compared to the other colours on the scale
  }
  return(na.value)
}

## ggplot ----

#' @rdname wikitopo_scale
#' @export
scale_fill_wikitopo <- function(na.value=NULL, ...) {
  ggplot2::scale_fill_gradientn(..., colors=chroma::wikitopo$color, values=scales::rescale(chroma::wikitopo$altitude), limits=range(chroma::wikitopo$altitude), na.value=wikitopo_na(na.value))
}
#' @rdname wikitopo_scale
#' @export
scale_color_wikitopo <- function(na.value=NULL, ...) {
  ggplot2::scale_color_gradientn(..., colors=chroma::wikitopo$color, values=scales::rescale(chroma::wikitopo$altitude), limits=range(chroma::wikitopo$altitude), na.value=wikitopo_na(na.value))
}
#' @rdname wikitopo_scale
#' @export
#' @usage NULL
scale_colour_wikitopo <- scale_color_wikitopo

