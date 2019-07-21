#' ggplot2 x and y scales definitions for maps
#'
#' Define nice looking x and y axes conventions for maps plotted with ggplot2
#'
#' @param ... passed to \code{\link[ggplot2]{scale_x_continuous}} \strong{and} \code{\link[ggplot2]{scale_y_continuous}}
#' @details This sets \code{expand=c(0,0)} to maximum map extent, map-py axes titles and labels, and removes minor breaks to be consistent among \code{\link[ggplot2]{coord_quickmap}} and \code{\link[ggplot2]{coord_map}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("ggplot2")
#' ggplot(thaixyz) + coord_quickmap() +
#'   geom_raster(aes(x=x, y=y, fill=z)) +
#'   scale_fill_wikitopo() +
#'   scale_xy_map()
#'  }
scale_xy_map <- function(...) {
  list(
    ggplot2::scale_x_continuous("Longitude",
      expand=c(0,0), minor_breaks=NULL, labels=function(x) {
      paste0(abs(x), "\u00B0", c("W","","E")[sign(x)+2])
    }, ...),
    ggplot2::scale_y_continuous("Latitude",
      expand=c(0,0), minor_breaks=NULL, labels=function(x) {
      paste0(abs(x), "\u00B0", c("S","","N")[sign(x)+2])
    }, ...)
  )
}