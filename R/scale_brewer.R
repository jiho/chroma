#' Colors from colorbrewer2.org
#'
#' @format A data.frame with 1680 rows and 4 variables:
#' \describe{
#'   \item{name}{the name of the color palette.}
#'   \item{type}{the type of palette (\code{qualitative}, \code{diverging}, or \code{sequential}).}
#'   \item{n}{the number of colors in the palette. For a given palette, all combinations of colors are hand picked and not just interpolated between the two extremes. So, even in the same palette, the colors for the different values of \code{n} may be slightly different.}
#'   \item{color}{the hex code of the color.}
#' }
#' @source \url{http://colorbrewer2.org}
#' @seealso \code{\link{brewer_info}} for a summary of the properties of colorbrewer palettes.
"brewer"

#' Description of the colorbrewer2.org color palettes
#'
#' @format A data.frame with 35 rows and 3 variables:
#' \describe{
#'   \item{type}{the type of palette (\code{qualitative}, \code{diverging}, or \code{sequential}).}
#'   \item{name}{the name of the color palette.}
#'   \item{maxcolors}{the maximum number of handpicked colors in the palette. (NB: the minimum is always 3).}
#' }
#' @source \url{http://colorbrewer2.org}
#' @seealso \code{\link{brewer}} for a complete table of colorbrewer2 colors.
"brewer_info"


#' ColorBrewer scales and palettes
#'
#' @param name name of a ColorBrewer palette. See \code{\link{brewer_info}} for a list of palettes and their characteristics.
#' @inheritParams interp_scale
#'
#' @template return_scales
#'
#' @family color scales and palettes
#'
#' @references \url{http://colorbrewer2.org}
#' @seealso \code{\link{brewer}} for the colors in the palettes and \code{\link{brewer_info}} for a list of palettes and their characteristics.
#'
#' @examples
#' # Define a scale function
#' ygb <- brewer_scale(name="YlGnBu")
#' ygb(c(0, 0.2, 0.6, 1))
#'
#' # Define a palette function
#' bgy_pal <- brewer_palette(name="YlGnBu", reverse=TRUE)
#' bgy_pal(10)
#' show_col(bgy_pal(100))
#'
#' # Show 7 colors from each palette
#' show_col(lapply(brewer_info$name, function(x) {brewer_colors(n=7, name=x)}))
#'
#' # Warn about the potentially inappropriate use of many colors
#' brewer_colors(n=15, name="Blues")
#' brewer_colors(n=15, name="Pastel1")
#' brewer_palette(name="Pastel1")(15)
#' # Some warnings can be avoided by explicitly requiring a palette
#' # which, by definition, is taken from a *continuous* scale
#' brewer_palette(name="Blues")(15)
#' brewer_palette(name="Pastel1")(15)
#'
#' # Sequential ColorBrewer palettes are good for continuous variables
#' # such as the elevation of the Maunga Whau volcano
#' image(maunga, col=brewer_colors(100, name="YlOrBr", reverse=TRUE))
#' contour(maunga, col=alpha("white", 0.5), add=TRUE)
#'
#' persp(maunga, theta=50, phi=25, scale=FALSE, expand=2,
#'       border=alpha("black", 0.4),
#'       col=brewer_map(persp_facets(maunga$z), "YlOrBr", reverse=TRUE))
#'
#' \dontrun{
#' library("rgl")
#' persp3d(maunga, aspect=c(1,0.7,0.2), axes=FALSE, box=FALSE,
#'         col=brewer_map(maunga$z, "YlOrBr", reverse=TRUE))
#'
#' }
#' # Qualitative palettes are appropriate for discrete variables
#' attach(iris)
#' plot(Petal.Length, Sepal.Length, pch=19, col=brewer_map(Species, "Set2"))
#' legend(1, 8, legend=levels(Species), pch=19,
#'        col=brewer_colors(n=nlevels(Species), name="Set2"))
#'
#' @export
brewer_scale <- function(name="Blues", model="lab", interp="linear", domain=c(0,1), reverse=FALSE) {
  # get 7 or so brewer colors
  colors <- brewer_colors(n=7, name=name)

  # check arguments
  if (chroma::brewer_info[name,"type"] == "qualitative") {
    warning("  Interpolating a continuous scale from a qualitative color palette is likely wrong.", call.=FALSE)
  }

  interp_scale(colors, model=model, interp=interp, domain=domain, reverse=reverse)
}

#' @param ... passed to \code{\link{brewer_scale}}. Note that argument \code{domain} is meaningless in functions other than \code{brewer_scale} and passing it through \code{...} is an error.
#' @rdname brewer_scale
#' @export
brewer_map <- function(x, ...) {
  # TODO better document this bit which is quite specific to colorbrewer having both discrete and continuous palettes
  # TODO this should really be in the scale function above, as a function f
  if (is.factor(x) | is.character(x)) {
    x <- factor(x)
    colors <- brewer_colors(n=nlevels(x), ...)[as.numeric(x)]
    # TODO center diverging palettes
  } else {
    x <- as.numeric(x)
    colors <- brewer_scale(domain=range(x, na.rm=T), ...)(x)
  }
  return(colors)
}

#' @rdname brewer_scale
#' @export
brewer_palette <- function(name="Blues", ...) {
  f <- function(n) {
    brewer_scale(name=name, domain=c(0,1), ...)(seq(0, 1, length.out=n))
  }
  return(f)
}

#' @rdname brewer_scale
#' @export
brewer_colors <- function(n, name="Blues", ...) {
  # check arguments and allow abbreviation
  name <- match.arg(name, chroma::brewer_info$name)

  # check the maximum numnber of colors for this palette
  maxn <- chroma::brewer_info[name,"maxcolors"]

  # if n colors are defined by colorbrewer2 return those
  if (n <= maxn) {
    # get the colors
    colors <- chroma::brewer$color[chroma::brewer$name == name & chroma::brewer$n == n]
    # put them through a scale to be able to use other arguments (such as a reverse, etc.)
    colors <- interp_palette(colors, ...)(n)
  }
  # otherwise interpolate colors (and warn about it)
  else {
    # warning("  n = ", n, " is larger than number of colors defined for palette \"", name, "\" (n = ", maxn, ").\n  New colors have been interpolated but the result is not guaranteed to be perceptually correct.", call.=FALSE)
    colors <- brewer_palette(name=name, ...)(n)
  }

  return(colors)
}

# TODO add ggplot2 functions with brewer_c and brewer_d variants as alternatives to scale_distiller and scale_brewer in ggplot2

