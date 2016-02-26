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

