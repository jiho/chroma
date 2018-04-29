#' Topographic information on Auckland's Maunga Whau volcano
#'
#' Maunga Whau (Mt Eden) is one of about 50 volcanos in the Auckland volcanic field. This data set gives topographic information for Maunga Whau on a 10m by 10m grid.
#'
#' @format \code{maunga} is a list with two coordinate vectors and a matrix of corresponding values, suitable for \code{\link[graphics]{contour}}, \code{\link[graphics]{persp}} etc. \code{maungaxyz} contains the same data but formated as a xyz a data.frame with 5307 rows and 3 columns. Both have components:
#' \describe{
#'   \item{x,y}{x and y coordinates, in m.}
#'   \item{z}{altitudes, in m.}
#' }
#' @seealso \code{\link[datasets]{volcano}} for the original data.
"maunga"

#' @rdname maunga
"maungaxyz"


#' Topographic data around Thailand
#'
#' Topography of the region around Thailand, featuring high mountains and deep trenches, at 0.2° resolution.
#'
#' @format \code{thai} is a list with two coordinate vectors and a matrix of corresponding values, suitable for \code{\link[graphics]{contour}}, \code{\link[graphics]{persp}} etc. \code{thaixyz} contains the same data but formated as a xyz a data.frame with 30351 rows and 3 columns. Both have components:
#' \describe{
#'   \item{x,y}{x and y coordinates, in degrees.}
#'   \item{z}{altitudes, in m.}
#' }
#' @source Extracted from NOAA's NGDC database (\url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}) and regridded.
"thai"

#' @rdname thai
"thaixyz"
