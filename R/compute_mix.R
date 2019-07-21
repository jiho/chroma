#' Mix two colors
#'
#' @template param_xy_rcolors
#' @param ratio amount of y mixed in x; \code{ratio} = 0 means pure \code{x}, \code{ratio} = 1 means pure \code{y}.
#' @param model string defining the color model to compute the mix in; valid models are \code{hcl}, \code{lch}, \code{hsi}, \code{hsl}, \code{hsv}, \code{lab}, \code{rgb}, \code{lrgb}.
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
#' mix("red", "blue")
#' mix("red", "blue", 0.25)
#' # Arguments can be vectors and all mixes are returned
#' mix("red", "blue", c(0.25, 0.5, 0.75))
#'
#' show_mix <- function(x, y, ...) {show_col(c(x, y, "white", mix(x, y, ...)))}
#' show_mix("#FEF213", "#146EFD")
#' show_mix("#FEF213", "#FA000C")
#' show_mix("#146EFD", "#FA000C")
#' show_mix("#146EFD", "#FA000C", ratio=c(0.25, 0.5, 0.75))
#' # not that results in some color spaces can be surprising
#' show_mix("#FEF213", "#146EFD",
#'          model=c("hcl", "lch", "hsi", "hsl", "hsv", "lab", "rgb", "lrgb"))
mix <- function(x, y, ratio=0.5, model="rgb") {
  # force input R colors into hex notation
  x <- in_hex(x)
  y <- in_hex(y)

  # check arguments values
  is_in(ratio, 0, 1, "ratio")
  model <- match.arg(model, c("hcl", "lch", "hsi", "hsl", "hsv", "lab", "rgb", "lrgb"), several.ok=TRUE)

  # check arguments lengths
  foo <- tabularise_arguments(x, y, ratio, model)
  # NB: foo is not used, we put them in a table just to check that their lengths are compatible

  # mix colors
  cmds <- stringr::str_c("chroma.mix('", x, "','", y, "', ", ratio, ", '", model, "').hex()")
  v8_eval(cmds)
}
# TODO compute mix in ryb color space; need to implement it manually.
