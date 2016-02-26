#' @details
#' The \code{*_scale} version creates a \strong{function} that takes a single argument (\code{x}: a numeric vector), maps the values to colors along an interpolated gradient and returns those colors as hex codes.
#'
#' The \code{*_palette} version also creates a \strong{function}, but it takes an integer, \code{n}, as argument, picks \code{n} colors evenly spaced along the scale and returns them as hex codes.
#'
#' The \code{*_colors} and \code{*.colors} versions are just shortcuts for \code{*_palette()(n)} and directly return \code{n} evenly spaced colors. They are equivalent to builtin functions such as \code{\link[grDevices]{heat.colors}}, \code{\link[grDevices]{topo.colors}}, etc.
#'
#' The \code{*_map} version is also a shortcut, which automatically creates a scale that spans all values in its argument \code{x}, maps the content of \code{x} on that scale and returns the colors. It is particularly useful as the \code{col} argument of plotting functions.

#' @return
#' \code{*_scale} and \code{*_palette} return a function.
#'
#' \code{*_colors}, \code{*.colors}, and \code{*_map} return a vector of hex colors.
