
#' @return
#' \code{*_scale} returns a \strong{function}. This function takes a single argument (\code{x}: a numeric vector), maps its values to colors, and returns thee colors as hex codes.
#'
#' \code{*_map} is a shortcut for \code{*_scale(domain=range(x))(x)}: it creates a scale that spans the range of values in argument \code{x}, maps the content of \code{x} on that scale, and returns the \strong{colors}.
#'
#' \code{*_palette} returns a \strong{function}. This function takes an integer (\code{n}) as argument, picks \code{n} colors evenly spaced along the scale, and returns them as hex codes.
#'
#' \code{*_colors} is a shortcut for \code{*_palette()(n)} and directly returns \code{n} evenly spaced \strong{colors}. It is equivalent to built-in functions such as \code{\link[grDevices]{heat.colors}}, \code{\link[grDevices]{topo.colors}}, etc.
#'
#' \code{scale_*} return a \strong{ggplot2 scale}, either discrete (similar to \code{\link[ggplot2]{scale_color_discrete}}) or continuous (similar to \code{\link[ggplot2]{scale_color_continuous}}).
