#' Put the legend on the side of a plot
#'
#' \code{sidemargin} prepares the margin, \code{sidelegend} puts the legend in it.
#'
#' @param ... passed to \code{\link[graphics]{legend}}.
#'
#' @return \code{sidemargin} returns the previous \code{\link[graphics]{par}} settings, so that they can be reset for futher plots.
#' \code{sidelegend} returns the same value as \code{\link[graphics]{legend}}.
#'
#' @export
#'
#' @examples
#' # Make a basic plot
#' attach(iris)
#' plot(Petal.Length, Petal.Width, col=Species, pch=19)
#' legend(1, 2, legend=levels(Species), col=1:nlevels(Species), pch=19)
#'
#' # The same with the legend on the side
#' pars <- sidemargin()
#' plot(Petal.Length, Petal.Width, col=Species, pch=19)
#' sidelegend(legend=levels(Species), col=1:nlevels(Species), pch=19)
#' par(pars) # reset graphical parameters
sidelegend <- function(...) {
  graphics::legend(
    # on top right with no box
    x=graphics::par("usr")[2], y=graphics::par("usr")[4], bty="n",
    # expand ploting region
    xpd=NA,
    # legend content
    ...
  )
}

#' @rdname sidelegend
#' @export
sidemargin <- function() {
  # save defaults
  pars <- graphics::par(no.readonly=TRUE)
  # add space on right, for legend
  graphics::par(oma=c(0,0,0,4))
  return(invisible(pars))
}
