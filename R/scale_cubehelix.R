#' CubeHelix color scale and palette
#'
#' The CubeHelix color palette, by Dave Green (Unlicense license).
#'
#' @template param_hue
#' @param rotations number of rotations of the helix in RGB space, in \code{[-2,2]} (1 = 360º, -1.5 = -540º)
#' @template param_chromacity
#' @template param_lightness
#' @param gamma gamma contrast factor, in \code{[0,+Inf]} (sane values are in \code{[0.5,3]}). \code{gamma} < 1 emphasises the low end of the scale by whitening the high colors. \code{gamma} > 1 emphasises the high end of the scale by darkening the low colors. Use with caution and restraint.
#' @inheritParams color_scale
#'
#' @template return_scales
#'
#' @family color scales and palettes
#'
#' @references \url{https://www.mrao.cam.ac.uk/~dag/CUBEHELIX/}
#' @examples
#' # Basic color palettes
#' show_col(
#'   cubehelix_colors(20),
#'   cubehelix_colors(20, reverse=T)
#' )
#'
#' # Rotating less gives more sensible scales
#' show_col(
#'   cubehelix_colors(20, h=300, rot=-0.75),
#'   cubehelix_colors(20, h=120, rot=0.5),
#'   cubehelix_colors(20, h=300, rot=0.5)
#' )
#'
#' # Examine the effect of arguments
#' show_col(
#'   cubehelix_colors(20, h=300, rot=0.5),
#'   # range of lightness
#'   cubehelix_colors(20, h=300, rot=0.5, l=c(0.1, 0.9)),
#'   # value of chromacity
#'   cubehelix_colors(20, h=300, rot=0.5, l=c(0.1, 0.9), c=1),
#'   # and gamma contrast factor
#'   cubehelix_colors(20, h=300, rot=0.5, l=c(0.1, 0.9), c=1, gamma=0.5),
#'   cubehelix_colors(20, h=300, rot=0.5, l=c(0.1, 0.9), c=1, gamma=1.5)
#' )
#'
#' # Plot the Maunga Whau volcano elevation map, a continuous variable
#' image(maunga, col=cubehelix_colors(100, rot=-0.75))
#' contour(maunga, col=alpha("black", 0.2), add=TRUE)
#'
#' persp(maunga, theta=50, phi=25, border=alpha("black", 0.3),
#'       col=cubehelix_map(persp_facets(maunga$z), rot=-0.75))
#'
#' \dontrun{library("rgl")
#' persp3d(maunga, aspect=c(1,0.6,0.3), axes=FALSE, box=FALSE,
#'         col=cubehelix_map(maunga$z, rot=-0.75))
#' play3d(spin3d(axis=c(0, 0, 1), rpm=10), duration=6)
#' }
#' # For discrete variables, using saturated colors along a scale of
#' # constant lightness gives a good hue-only scale
#' attach(iris)
#' # Define the color scale
#' ch_discrete <- cubehelix_scale(
#'   h=0, rot=0.75, # Start from red-ish and do not make a full circle
#'                  # to avoid falling back on red
#'   c=1,           # Use saturated colors
#'   l=c(0.7, 0.7), # Do not vary lightness
#'   domain=c(1,nlevels(Species)) # This will map to a factor with 3 levels
#' )
#' 
#' plot(Petal.Length, Sepal.Length, pch=19,
#'      col=ch_discrete(as.numeric(Species))) # Convert factor to numeric
#' legend(1, 8, legend=levels(Species), pch=19,
#'        col=ch_discrete(1:3))
#' # But see ?hue_scale for a simpler solution
#'
#' @export
cubehelix_scale <- function(h=300, rotations=-1.5, c=0.5, l=c(0.1, 0.9), gamma=1, domain=c(0,1), reverse=FALSE) {

  # check arguments
  if (length(h) != 1) { stop("h should be just one value.") }
  if (length(rotations) != 1) { stop("rotations should be just one value.") }
  if (length(c) != 1) { stop("c should be just one value.") }
  if (length(l) != 2) { stop("l should be a vector of length 1 or 2.") }
  if (length(gamma) != 1) { stop("gamma should be just one value.") }
  is_in(rotations, -2, 2)
  is_in(c, 0, 1.5)
  is_in(l, 0, 1)
  is_in(gamma, 0, 2)

  # if lightness bounds are identical, make them slightly different
  if (l[1] == l[2]) {
    l[1] <- max(0, l[1]-10^-6)
    l[2] <- min(1, l[2]+10^-6)
  }

  # parse hue (in case it is provided as a color)
  h <- hue(h)
  # scale chromacity up to make the colors bright enough (but keep c in [0,1.5], as the documentation says)
  c <- c * 2

  # prepare chroma.js command
  if ( reverse ) {
    domain <- rev(domain)
  }
  domaint <- stringr::str_c("[",stringr::str_c(domain, collapse=","),"]")
  lt <- stringr::str_c("[",stringr::str_c(l,collapse=","),"]")

  # define function
  eval(f <- function(x) {
    cmds <- stringr::str_c("chroma.cubehelix().start(", h, ").rotations(", rotations, ").hue(", c, ").lightness(", lt, ").gamma(", gamma, ").scale().domain(", domaint, ").mode('rgb')(", x, ").hex()")
    v8_eval(cmds)
  })
}

#' @param ... passed to \code{\link{cubehelix_scale}}. Note that argument \code{domain} is meaningless in functions other than \code{cubehelix_scale} and passing it through \code{...} is an error.
#' @name cubehelix_scale
#' @export
cubehelix_map <- function(x, ...) {
  # force characters into factors to be able to convert them to numeric
  if (is.character(x)) {
    x <- factor(x)
  }
  # convert to numbers
  x <- as.numeric(x)
  # define the domain of the scale
  cubehelix_scale(domain=range(x, na.rm=T), ...)(x)
}

#' @name cubehelix_scale
#' @export
cubehelix_palette <- function(n, ...) {
  f <- function(n) {
    cubehelix_scale(domain=c(0,1), ...)(seq(0, 1, length.out=n))
  }
  return(f)
}

#' @name cubehelix_scale
#' @export
cubehelix_colors <- function(n, ...) {
  cubehelix_palette(...)(n)
}
#' @name cubehelix_scale
#' @export
cubehelix.colors <- cubehelix_colors
