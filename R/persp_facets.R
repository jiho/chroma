#' Facets values in a persp plot
#'
#' Compute the value at the center of facets in a perspective plot of a matrix
#'
#' @param z a numeric matrix to be used as the z argument of \code{link[graphics]{persp}}
#'
#' @return A matrix of dimensions \code{[nrow(z)-1, ncol(z)-1]} with the values at the center of each facet
#'
#' @seealso \code{\link{interp_scale}} for an example of usage.
#'
#' @examples
#' z <- matrix(1:12, nrow=4)
#' persp_facets(z)
#'
#' # use it with persp()
#' persp(maunga, theta=50, phi=25, scale=FALSE, expand=2,
#'       border=alpha("black", 0.4),
#'       col=interp_map(persp_facets(maunga$z), colors=terrain.colors(10)))
#'
#' @export
persp_facets <- function(z) {
  if (!is.matrix(z) | !is.numeric(z)) {
    stop("z needs to be a numeric matrix")
  }
  ncz <- ncol(z)
  nrz <- nrow(z)
  # value at facet center
  zf <- (z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]) / 4
  return(zf)
}
