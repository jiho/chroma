#' @return
#' The \code{*_scale} version returns a function that takes \code{x}, a numeric vector, as argument, maps the values to colors along the interpolated gradient and returns those colors as hex codes.
#'
#' The \code{*_palette} version returns a function that takes \code{n}, an integer number, as argument, picks \code{n} colors evenly along the scale and returns them as hex codes.
#'
#' The \code{*_colors} and \code{*.colors} versions are just shortcuts for \code{*_palette()(n)} and directly return \code{n} colors evenly spaced along the scale, as hex codes. It is meant to be equivalent to builtin functions such as \code{\link[grDevices]{heat.colors}}, \code{\link[grDevices]{topo.colors}}, etc.
