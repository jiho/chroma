#' Blend two colors
#'
#' Blends two colors using RGB channel-wise blend functions.
#'
#' @param x,y colors specified as hex strings or named R colors.
#' @param mode blending mode; valid modes are \code{multiply}, \code{darken}, \code{lighten}, \code{screen}, \code{overlay}, \code{burn}, and \code{dodge}.
#'
#' @details
#' All arguments can be vectors, as long as they are of compatible length (shorter arguments will be repeated to the length of longer arguments)
#'
#' @template return_hex_colors
#'
#' @family color manipulation functions
#'
#' @export
#'
#' @examples
#' blend("#4CBBFC", "#EEEE22")
#' # Arguments can be vectors and all blends are returned
#' mix("red", c("blue", "darkblue", "lightblue"))
#'
#' #' Show the effects of the various blend modes
#' all_blends <- function(x, y) {
#'   c(x, y,
#'     "white",
#'     blend(x, y, mode=c("multiply", "darken", "lighten",
#'                        "screen", "overlay", "burn", "dodge"))
#'    )
#' }
#' show_col(
#'   all_blends("#FEF213", "#146EFD"),
#'   all_blends("#FEF213", "#FA000C"),
#'   all_blends("#146EFD", "#FA000C")
#' )
blend <- function(x, y, mode="multiply") {
  # force input R colors into hex notation
  x <- in_hex(x)
  y <- in_hex(y)

  # check arguments values
  mode <- match.arg(mode, c("multiply", "darken", "lighten", "screen", "overlay", "burn", "dodge"), several.ok=TRUE)

  # check arguments lengths
  foo <- tabularise_arguments(x, y, mode)
  # NB: foo is not used, we put them in a table just to check that their lengths are compatible

  # mix colors
  cmds <- stringr::str_c("chroma.blend('", x, "','", y, "', '", mode, "').hex()")
  v8_eval(cmds)
}
