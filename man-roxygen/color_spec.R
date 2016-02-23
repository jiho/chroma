#' @details
#' The first argument can also be a data.frame or a matrix. In that case, its columns are considered as the color components, taken in order, and the other color components arguments are ignored.
#'
#' When separate arguments are used for the color components and are vectors, values in shorter arguments are recycled to match the length of the longest argument. If the lengths are not compatible, an error is output.
#'
#' @family color specifications
#'
#' @return A vector of colors specified as hex codes
#' 
#' @seealso See \code{\link{parse_color}} for the general function to parse colors in various specifications (which this function calls internally) and \code{\link{convert_color}} to convert parsed colors to another model.

