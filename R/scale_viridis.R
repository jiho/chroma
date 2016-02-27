#' Viridis colors
#'
#' The \code{viridis} color palette, by Nathaniel J. Smith, Stefan van der Walt, and Eric Firing (CC0 license).
#'
#' @format A vector of length 256 containing hex values.
#' @source \url{https://bids.github.io/colormap/} for the concept and \url{https://github.com/BIDS/colormap/blob/master/colormaps.py} for the data.
#' @family viridis-like palettes
"viridis"


#' Viridis color scale and palette
#'
#' The \code{viridis} color palette, by Nathaniel J. Smith, Stefan van der Walt, and Eric Firing (CC0 license).
#'
#' @inheritParams make_scale 
#'
#' @template return_scales
#'
#' @family color scales and palettes
#'
#' @examples
#' # get a few colors along the palette
#' show_col(
#'   viridis_palette()(20),
#'   viridis_colors(50),
#'   viridis_colors(20, reverse=TRUE)
#' )
#' 
#' # Maunga Whau volcano elevation map
#' x <- 10*(1:nrow(volcano))
#' y <- 10*(1:ncol(volcano))
#' image(x, y, volcano, col=viridis_colors(100))
#' persp(x, y, volcano, theta=60, phi=25, border=NA,
#'       col=viridis_map(persp_facets(volcano)))
#' \dontrun{
#' library("rgl")
#' persp3d(x, y, volcano, aspect=c(1,0.6,0.3), axes=FALSE, box=FALSE,
#'         col=viridis_map(volcano))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#'
#' }
#' # with a limited number of levels, viridis can also serve as a discrete
#' # color scale
#' attach(iris)
#' plot(Petal.Length, Sepal.Length, pch=19, col=viridis_map(Species))
#' legend(1, 8, legend=levels(Species), pch=19, col=viridis_colors(n=nlevels(Species)))
#'
#' @export
viridis_scale <- function(domain=c(0,1), reverse=FALSE) {
  make_scale(colors=chroma::viridis, model="lab", interp="linear", domain=domain, reverse=reverse)
}

#' @param ... passed to \code{\link{viridis_scale}}. Note that argument \code{domain} is meaningless in functions other than \code{viridis_scale} and passing it through \code{...} is an error.
#' @name viridis_scale
#' @export
viridis_palette <- function(...) {
  make_palette(colors=chroma::viridis, model="lab", interp="linear", ...)
}

#' @name viridis_scale
#' @export
viridis_colors <- function(n, ...) {
  viridis_colors(n=n, colors=chroma::viridis, model="lab", interp="linear",...)
}
#' @name viridis_scale
#' @export
viridis.colors <- viridis_colors

#' @name viridis_scale
#' @export
viridis_map <- function(x, ...) {
  make_map(x, colors=chroma::viridis, model="lab", interp="linear", ...)
}
