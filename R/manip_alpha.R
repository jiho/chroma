#' Modify color transparency
#'
#' Modify color transparency, vectorized in both \code{x} and \code{alpha} (when both are vectors, they must be the same length).
#'
#' @details
#' Most other operations in chroma do not support alpha channels (because the underlying library, chroma.js, does not output 8 digits hex color specifications, which describe r, g, b, and a channels). Therefore, any manipulation of transparency must be done last.
#'
#' @template param_x_rcolors
#' @param alpha transparency, number in \code{[0, 1]}; 0 means fully transparent, 1 means fully opaque.
#'
#' @export
#'
#' @examples
#' as.hex("red")
#' alpha("red")
#'
#' show_col(c("red", alpha("red")))
#'
#' # Vectorised in both x and alpha, as long as the lengths are compatible
#' alpha(c("red", "green", "blue"), 0.5)
#' alpha("red", c(0.2, 0.5, 0.7))
#' alpha(c("red", "green", "blue"), c(0.2, 0.5, 0.7))
#'
#' \dontrun{
#'   alpha(c("red", "green", "blue", "purple"), c(0.2, 0.5))
#'   alpha(c("red", "green", "blue", "purple"), c(0.2, 0.5, 0.7))
#'   # = fail because arguments have incompatible lengths
#' }
#' # Beware, other functions do not support alpha channels
#' mix(alpha("red"), alpha("blue"))
#' mix("red", "blue")
#' # = both are the same, and fully opaque. Use alpha last
#' alpha(mix("red", "blue"))
alpha <- function(x, alpha=0.5) {
  # check arguments
  is_in(alpha, 0, 1)
  tabularise_arguments(x, alpha)
  # NB: used to check length here, only

  # force input R colors into hex notation
  x <- in_hex(x)
  # add alpha channel bit at the end
  stringr::str_c(x, format(as.hexmode(round(alpha*255)), width=2, upper.case=TRUE))
}
