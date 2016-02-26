#' Show colors
#'
#' Display one or several (collections of) colors next to each other
#'
#' @param ... vectors of colors (specified as hex codes) to display. Each vector is displayed on a separate line.
#'
#' @examples
#' # display one color
#' show_col("#ff3399")
#'
#' # display, and compare, color collections
#' show_col(c("#ff3399", "#9c0c55"))
#' show_col(c("#ff3399", "#9c0c55"), c("#2976fe", "#0c2c8a"))
#'
#' # useful to compare color palettes
#' show_col(grey.colors(20))
#' show_col(grey.colors(20), rainbow(20), heat.colors(15),
#'          terrain.colors(6), topo.colors(10), cm.colors(20))
#' jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF",
#'   "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#' show_col(rev(rainbow(20, start=0, end=0.65)), jet.colors(20))
#' # ewwww :-(
#'
#' @export
#' @importFrom grid grid.newpage grid.layout viewport pushViewport grid.rect gpar unit
show_col <- function(...) {
  # get vectors of colors to plot
  args <- list(...)
  
  # deal with the special case of ... being a single list of colors
  if ( length(args) == 1 & is.list(args[[1]]) ) {
    args <- args[[1]]
  }

  # count the number of color collections
  nn <- length(args)

  # prepare a new page with as many lines as color vectors
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nn, 1)))

  # plot each color vectors
  for (i in 1:length(args)) {
    # get color vector
    x <- args[[i]]
    
    # count colors
    n <- length(x)

    # plot them as rectangles
    grid.rect(
      x=unit((1:n-0.5)/n, "npc"),
      width=unit(1/n, "npc"),
      # height=unit(1, "npc"),
      gp=gpar(col=NA, fill=x),
      vp=viewport(layout.pos.row=i, layout.pos.col=1)
    )
  }
}
