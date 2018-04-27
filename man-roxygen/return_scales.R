#' @details
#' As a user, the most useful functions are \code{*_map} and \code{*_colors}.
#'
#' \code{*_scale} creates a \strong{function} that takes a single argument (\code{x}: a numeric vector), maps its values to colors, and returns those colors as hex codes.
#'
#' \code{*_map} is a shortcut for \code{*_scale(domain=range(x))(x)}: it creates a scale that spans all values in its argument \code{x}, maps the content of \code{x} on that scale, and returns the colors. It is particularly useful as the \code{col} argument of plotting functions.
#'
#' \code{*_palette} also creates a \strong{function}, but this one takes an integer (\code{n}) as argument, picks \code{n} colors evenly spaced along the scale, and returns them as hex codes.
#'
#' \code{*_colors} is a shortcut for \code{*_palette()(n)} and directly returns \code{n} evenly spaced colors. It is equivalent to built-in functions such as \code{\link[grDevices]{heat.colors}}, \code{\link[grDevices]{topo.colors}}, etc.
#'

#' @return
#' \code{*_scale} and \code{*_palette} return a function.
#'
#' \code{*_map} and \code{*_colors} return a vector of hex colors.
