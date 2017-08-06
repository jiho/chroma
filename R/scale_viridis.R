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
#' # Plot the Maunga Whau volcano elevation map
#' image(maunga, col=viridis_colors(100))
#' contour(maunga, col=alpha("black", 0.2), add=TRUE)
#'
#' persp(maunga, theta=50, phi=25, border=alpha("black", 0.3),
#'       col=viridis_map(persp_facets(maunga$z)))
#'
#' \dontrun{library("rgl")
#' persp3d(maunga, aspect=c(1,0.6,0.3), axes=FALSE, box=FALSE,
#'         col=viridis_map(maunga$z))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#' }
#' # With a limited number of levels, viridis can also serve as a discrete
#' # color scale
#' attach(iris)
#' plot(Petal.Length, Sepal.Length, pch=19, col=viridis_map(Species))
#' legend(1, 8, legend=levels(Species), pch=19,
#'        col=viridis_colors(n=nlevels(Species)))
#'
#' @export
viridis_scale <- function(domain=c(0,1), reverse=FALSE) {
  interp_scale(colors=chroma::viridis, model="lab", interp="linear", domain=domain, reverse=reverse)
}

#' @param ... passed to \code{\link{viridis_scale}}. Note that argument \code{domain} is meaningless in functions other than \code{viridis_scale} and passing it through \code{...} is an error.
#' @rdname viridis_scale
#' @export
viridis_map <- function(x, ...) {
  interp_map(x, colors=chroma::viridis, model="lab", interp="linear", ...)
}

#' @rdname viridis_scale
#' @export
viridis_palette <- function(...) {
  interp_palette(colors=chroma::viridis, model="lab", interp="linear", ...)
}

#' @rdname viridis_scale
#' @export
viridis_colors <- function(n, ...) {
  interp_colors(n=n, colors=chroma::viridis, model="lab", interp="linear",...)
}
